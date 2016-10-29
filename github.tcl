# github.tcl --
#
#     This script receives GitHub webhook events. It creates a partial HTTPd
#     and waits for events getting pushed to it from GitHub. You need to have
#     access to the webhook settings of the repository you wish to get
#     notifications from on GitHub. Scroll down to change some basic settings.
#     The majority of the settings for this script are available via the
#     ".github" command from the partyline.
#
# Copyright (c) 2016, Rickard Utgren <rutgren@gmail.com>
#
# Permission to use, copy, modify, and/or distribute this software for any
# purpose with or without fee is hereby granted, provided that the above
# copyright notice and this permission notice appear in all copies.
#
# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
# WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
# ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
# WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
# ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
# OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
#
# v0.2 by Pixelz (rutgren@gmail.com), October 29, 2016
#
# Changes:
# v0.2. October 29, 2016:
#	- Added support for label event.
#	- Added support for milestoned & demilestoned actions for issue event.
#	- Added delayed output (10 seconds) for issue events. The script will now
#	  fit all changes done to an issue in the same announcement.
#	- Added support for status events.
#	- Added a setting to enable (off by default) showing individual commits to
#     a non-default branch.
#	- Added number of bytes changed in an issue comment edit.
#	- The script will now ignore push events for creating or deleting branches
#	  or tags since there's another event that handles those.
#	- More readable output for '.github list'
#	- Fixed the issue linker not linking to issues with really low numbers.
#	- Fixed issue comment event so that it doesn't always say "commented" even
#     if the action was edited or closed.
#	- Strip newlines from issue comments for more meaningful output.
# 	- Fixed problem that caused commits not to show if the committer didn't
#	  have an account on github.
#	- Fixed a bug that caused multiple repositories to not work properly.
#	- Fixed a bug that caused events to be sent to all channels that had that
#	  event configured.
#	
# v0.1. January 31, 2016:
#	- Initial release.
#
# ToDo:
#	- Write settings to temp file & move


# some of these might be able to go lower, these are what the script was tested on
package require Tcl 8.6 ;# 8.6 needed
package require eggdrop 1.6.21
package require http 2.8.8
package require tls 1.6.4
package require json 1.3.3 ;# I think 1.3.3 is needed
package require sha1 2.0.3
package require base64 2.4.2

namespace eval ::github {
	### Settings:

	# Listen port for the built-in HTTPd. You probably want to change this.
	# Changing this when the script is loaded requires a RESTART (not a rehash).
	set port 52525

	# This is how many channels the bot will output to in a single command. The
	# more that your IRC server supports, the better. You can figure this out
	# by sending "/quote VERSION" to the server and find the part starting with
	# "TARGMAX=" or possibly "MAXTARGETS=". If you're unsure, set it to 1. At
	# the time of writing, freenode supports 4.
	# Additionally, some networks have channel modes that prevent sending
	# multi-target PRIVMSGs. If the bot is on a channel with such a mode set,
	# set this to 1.
	set privmsgTargMax 4

	# Shorten URLs using git.io?
	set shortenUrls 1

	# Show "pending" status events. These are events showing the status of a
	# commit. They are sent via github's API from a 3rd party, for example
	# Jenkins, etc. Pending means that the commit has been queued for building
	# or similar. There will be other events sent after the pending one(s) that
	# are generally more interesting.
	set showPendingStatus 0
	
	# Show individual commits pushed to a non-default branch. This will
	# potentially result in walls of text whenever a branch is synchronized
	# with the default branch.
	set showNonDefaultCommits 0


	### More settings, you probably don't need to change these:

	# User-flags required to use the .github partyline command.
	# Note that ANY of these flags would be required (not all).
	# Changing this requires a restart.
	set dccFlags "n"

	# path to settings file
	set settingsFile "scripts/github.tcl.settings"

	# maximum length of payload in bytes
	set payloadLimit 10485760;# 10MB

	# maximum length of headers in bytes
	set headerLimit 5242880; # 5MB

	# store JSON payload to a file, for reviewing later
	set storeJson 0

	### End of settings


	# load local settings if they exist
	if {[file exists scripts/github.tcl.local] && [file readable scripts/github.tcl.local]} {
		source scripts/github.tcl.local
		putlog "Loaded local settings from github.tcl.local"
	}
	
	# ToDo: add support:
	# deployment
	# deployment_status
	# membership (organization hooks only)
	# milestone
	# page_build
	# public
	# pull_request_review
	# repository
	# team_add
	
	set validEvents [list commit_comment create delete fork gollum issue_comment issues label\
					member pull_request pull_request_review_comment push release status watch]

	variable state
	variable settings
	variable delayedSendIds
}

# rfc1459 channel name comparison
# Note: []\^ (uppers) == {}|~ (lowers)
proc ::github::ircstreql {string1 string2} {
	string equal -nocase [string map [list \{ \[ \} \] ~ ^ | \\] $string1] [string map [list \{ \[ \} \] ~ ^ | \\] $string2]
}

proc ::github::gitio {url} {
	variable shortenUrls
	if {[info exists shortenUrls] && $shortenUrls != 1} { return $url }

	set url [regsub -- {^http://} $url {https://}]
	set token [::http::geturl https://git.io -query [::http::formatQuery url $url]]
	upvar #0 $token state
	array set meta $state(meta)
	catch { ::http::cleanup $token }
	if {[string equal "201 Created" $meta(Status)] && [string match "https://git.io/*" $meta(Location)] && [string length $meta(Location)] > 15} {
		return $meta(Location)
	} else {
		putlog "github.tcl Error: git.io URL shortening failed."
		putlog "github.tcl git.io meta: [array get meta]"
		return $url
	}
}

proc ::github::issueLinker {nick uhost hand chan text} {
	variable settings
	# cheap way to filter out most things we don't care about
	if {(![string match -nocase {*#[0-9][0-9]*} $text]) && (![string match -nocase "*http*github.com*" $text])} {
		return 1
	}
	
	# check if this channel has a repo associated with it
	foreach {setChan repo} [dict get $settings linker] {
		if {[ircstreql $setChan $chan]} { set foundRepo 1; break }
	}
	if {![info exists foundRepo]} {
		return 1
	}
	putloglev d * "found repo for $chan: $repo"
	
	# extract IDs from the text
	foreach {- a b} [regexp -all -nocase -inline "(?:\#(\[0-9\]+)|https?://github.com${repo}/(?:issues|pull)/(\[0-9\]+))" $text] {
		if {![string equal "" $a]} {
			lappend ids $a
		} elseif {![string equal "" $b]} {
			lappend ids $b
		}
	}
	if {![info exists ids]} {
		return 1
	} else {
		set ids [lsort -unique $ids]
	}
	putloglev d * "found ID(s): $ids"
	
	# FixMe: make this a setting?
	if {[llength $ids] > 3} {
		putlog "github.tcl: $nick tried to flood me with [llength $ids] IDs. Not outputting."
		return 1
	}

	foreach id $ids {
		# https://api.github.com/repos/eggheads/eggdrop/issues/
		# pull request: https://api.github.com/repos/eggheads/eggdrop/issues/156
		# issue: https://api.github.com/repos/eggheads/eggdrop/issues/151
		set token [::http::geturl https://api.github.com/repos${repo}/issues/${id}]
		upvar #0 $token state
		
		set status [string tolower $state(status)]
		if {[info exists state(error)]} { set error $state(error) }
		array set meta $state(meta)
		set body [encoding convertfrom utf-8 $state(body)]
		catch { ::http::cleanup $token }
		
		switch -exact -- $status {
			reset {
				putlog "github.tcl Error: issueLinker http error: connection reset"
			}
			timeout {
				putlog "github.tcl Error: issueLinker http error: connection timeout"
			}
			error {
				putlog "github.tcl Error: issueLinker http error: $error"
			}
			ok {
				if {[catch {::json::json2dict $body} jdict]} {
					putlog "github.tcl Error: issueLinker error parsing json: $jdict"
					return 1
				} elseif {[dict exists $jdict message] && [string equal -nocase "not found" [dict get $jdict message]]} {
					return 1
				} else {
					if {[dict exists $jdict pull_request]} {
						# this is a pull request
						putserv "PRIVMSG $chan :[gitio [dict get $jdict html_url]] pull request #${id} \"[dict get $jdict title]\" ([dict get $jdict state])"
					} else {
						# this is an issue
						putserv "PRIVMSG $chan :[gitio [dict get $jdict html_url]] issue #${id} \"[dict get $jdict title]\" ([dict get $jdict state])"
					}
				}
			}
			default {
				putlog "github.tcl Error: issueLinker unknown http status: $status"
				return 1
			}
		}
	}
	return
}

proc ::github::plural {word count} {
	if {$count > 1} {
		return "${word}s"
	} else {
		return $word
	}
}

proc ::github::shortenBody {string {length 64}} {
	set string [string map [list \r ""] $string]
	set firstLine [lindex [split $string \n] 0]
	set retval [string range $firstLine 0 $length]
	if {[string length $firstLine] > $length || [llength [split $string \n]] > 1} {
		set retval "[string trim [string range $retval 0 ${length}-3]]..."
	}
	return $retval
}

proc ::github::formatRef {ref} {
	# refs/heads/master
	# refs/heads/bug/encodings
	# refs/tags/v1.8.0rc1
	if {[regexp -- {^refs/(?:heads|tags)/(.+)$} $ref - branch]} {
		return "/$branch"
	} else {
		return
	}
}

# this is unused at the moment
proc ::github::getRefType {ref} {
	# this is a branch: refs/heads/release/1.8.0
	# this is a tag: refs/tags/v1.8.0rc1
	if {[regexp -- {^refs/(head|tag)s/.+$} $ref - type]} {
		if {[string equal $type "head"]} { set type "branch" }
		return $type
	} else {
		return
	}
}

# by BEO http://wiki.tcl.tk/10874
proc ::github::format_1024_units {value} {
	set len [string length $value]
	if {$value < 1024} {
		format "%s [plural "byte" $value]" $value
	} else {
		set unit [expr {($len - 1) / 3}]
		format "%.1f %s" [expr {$value / pow(1024,$unit)}] [lindex [list B KiB MiB GiB TiB PiB EiB ZiB YiB] $unit]
	}
}

proc ::github::parseJson {event json} {
	variable showPendingStatus
	variable showNonDefaultCommits
	upvar 1 delayedSend delayedSend
	set jdict [::json::json2dict $json]
	putloglev d * "github.tcl: parseJson event: $event"
	switch -- $event {
		ping {
			putlog "github.tcl: Got ping event, ignoring"
		}
		commit_comment {
			set msg "\[[dict get $jdict repository name]\] [dict get $jdict sender login] "
			append msg "commented on commit [string range [dict get $jdict comment commit_id] 0 6]: "
			append msg "[shortenBody [dict get $jdict comment body]] "
			append msg "[gitio [dict get $jdict comment html_url]]"
			return [list $msg]
		}
		create {
			# branch or tag was created, webhooks will not receive the event for created repositories
			set msg "\[[dict get $jdict repository name]\] [dict get $jdict sender login] "
			append msg "created [dict get $jdict ref_type] [dict get $jdict ref]"
			if {[string equal [dict get $jdict description] ""]} {
				append msg " "
			} else {
				append msg ": [shortenBody [dict get $jdict description]] "
			}
			append msg "[gitio [dict get $jdict repository html_url]]"
			return [list $msg]
		}
		delete {
			# branch or tag was deleted
			set msg "\[[dict get $jdict repository name]\] [dict get $jdict sender login] "
			append msg "deleted [dict get $jdict ref_type] [dict get $jdict ref] "
			append msg "[gitio [dict get $jdict repository html_url]]"
			return [list $msg]
		}
		fork {
			set msg "\[[dict get $jdict repository name]\] [dict get $jdict sender login] "
			append msg "has forked [dict get $jdict repository full_name] "
			append msg "to [dict get $jdict forkee full_name]: "
			append msg "[gitio [dict get $jdict forkee html_url]]"
			return [list $msg]
		}
		gollum {
			# Triggered when a Wiki page is created or updated
			foreach page [dict get $jdict pages] {
				set msg "\[[dict get $jdict repository name]\] [dict get $jdict sender login] "
				append msg "[dict get $page action] wiki page [dict get $page page_name]: "
				append msg "[gitio [dict get $page html_url]]"
				lappend retval $msg
			}
			return $retval
		}
		issue_comment {
			set msg "\[[dict get $jdict repository name]\] [dict get $jdict sender login] "
			set bytes ""
			switch -- [dict get $jdict action] {
				created {
					set action "commented on"
				}
				edited {
					set oldLen [string bytelength [dict get $jdict changes body from]]
					set newLen [string bytelength [dict get $jdict comment body]]
					if {$oldLen > $newLen} {
						set bytes " ([format_1024_units [expr $oldLen - $newLen]] removed)"
					} elseif {$newLen > $oldLen} {
						set bytes " ([format_1024_units [expr $newLen - $oldLen]] added)"
					}
					set action "edited comment on"
				}
				deleted {
					set action "deleted comment on"
				}
				default {
					# this should not happen unless the API changes
					set action [dict get $jdict action]
				}
			}
			append msg "$action issue \#[dict get $jdict issue number]${bytes}: "
			# strip newlines for more meaningful output
			append msg "[shortenBody [join [regexp -all -inline {\S+} [dict get $jdict comment body]]]] "
			append msg "[gitio [dict get $jdict issue html_url]]"
			return [list $msg]
		}
		issues {
			set action [dict get $jdict action]

			set repName [dict get $jdict repository name]
			set issueNum [dict get $jdict issue number]
			set senderLogin [dict get $jdict sender login]
			set issueTitle [dict get $jdict issue title]
			set url [dict get $jdict issue html_url]
			set delayedSend 1
			switch -- $action {
				assigned -
				unassigned {
					return [list $repName $issueNum $action $senderLogin $issueTitle $url [dict get $jdict assignee login]]
				}
				labeled -
				unlabeled {
					return [list $repName $issueNum $action $senderLogin $issueTitle $url [dict get $jdict label name]]
				}
				milestoned {
					return [list $repName $issueNum $action $senderLogin $issueTitle $url [dict get $jdict issue milestone title]]
				}
				opened -
				closed -
				reopened -
				demilestoned {
					return [list $repName $issueNum $action $senderLogin $issueTitle $url]
				}
				default {
					# this should not happen unless new actions are added to the API
					set msg "\[${repName}\] $senderLogin $action issue #${issueNum} (${issueTitle}) [gitio $url]"
					set delayedSend 0
				}
			}
			return [list $msg]
		}
		label {
			set action [dict get $jdict action]
			set repName [dict get $jdict repository name]
			set senderLogin [dict get $jdict sender login]
			set labelName [dict get $jdict label name]
			set url [gitio [dict get $jdict repository html_url]]

			if {[string equal $action "edited"]} {
				if {[dict exists $jdict changes name from]} {
					# name was changed
					set msg "\[${repName}\] $senderLogin renamed label \"[dict exists $jdict changes name from]\" to \"${labelName}\" $url"
				} elseif {[dict exists $jdict changes color from]} {
					# color was changed
					set msg "\[${repName}\] $senderLogin changed the color of label \"${labelName}\" $url"
				}  else {
					# something else was changed that's not currently in the API
					set msg "\[${repName}\] $senderLogin $action label \"${labelName}\" $url"
				}
			} else {
				# created, deleted
				set msg "\[${repName}\] $senderLogin $action label \"${labelName}\" $url"
			}
			return [list $msg]
		}
		member {
			set msg "\[[dict get $jdict repository name]\] [dict get $jdict sender login] "
			append msg "[dict get $jdict action] member [dict get $jdict member login] "
			append msg "to [dict get $jdict repository full_name]: "
			append msg "[gitio [dict get $jdict repository html_url]]"
			return [list $msg]
		}
		pull_request {
			# If the action is "closed" and the merged key is "false", the pull request was closed with unmerged commits.
			# If the action is "closed" and the merged key is "true", the pull request was merged.
			set action [dict get $jdict action]
			if {[string equal $action "closed"] && [string equal [dict get $jdict pull_request merged] "true"]} {
				set action "merged"
			}
			
			# set description for some actions
			# this wording is a little weird but I can't come up with a better way of formulating it
			set actionDescription ""
			if {[string equal $action "assigned"]} {
				set actionDescription " to [dict get $jdict assignee login]"
			} elseif {[string equal $action "unassigned"]} {
				set actionDescription " from [dict get $jdict assignee login]"
			} elseif {[string equal $action "labeled"] || [string equal $action "unlabeled"]} {
				set actionDescription " with \"[dict get $jdict label name]\""
			}
			
			set msg "\[[dict get $jdict repository name]\] [dict get $jdict sender login] "
			append msg "$action pull request${actionDescription} #[dict get $jdict number]: "
			append msg "[dict get $jdict pull_request title] "
			append msg "([dict get $jdict pull_request base ref]...[dict get $jdict pull_request head ref]) "
			append msg "[gitio [dict get $jdict pull_request html_url]]"
			return [list $msg]
		}
		pull_request_review_comment {
			set msg "\[[dict get $jdict repository name]\] [dict get $jdict sender login] "
			append msg "commented on pull request #[dict get $jdict pull_request number]: "
			append msg "[shortenBody [dict get $jdict comment body]] "
			append msg "[gitio [dict get $jdict comment html_url]]"
			return [list $msg]
		}
		push {
			set msg "\[[dict get $jdict repository name]\] [dict get $jdict pusher name] "
			if {[string equal [dict get $jdict created] "true"]} {
				# this is a created branch or tag
				putloglev d * "github.tcl: got a push event for a created branch or tag - ignoring"
				return
				#append msg "created [getRefType [dict get $jdict ref]] [dict get $jdict repository name][formatRef [dict get $jdict ref]]"
				#lappend retval $msg
			} elseif {[string equal [dict get $jdict deleted] "true"]} {
				# this is a deleted branch or tag
				 putloglev d * "github.tcl: got a push event for a deleted branch or tag - ignoring"
				return
				#append msg "deleted [getRefType [dict get $jdict ref]] [dict get $jdict repository name][formatRef [dict get $jdict ref]]"
				#lappend retval $msg
			} else {
				# this is a normal push
				if {[llength [dict get $jdict commits]] == 0} {
					# no commits were pushed
					putloglev d * "github.tcl: got a push event with 0 commits - ignoring"
					return
				}
				append msg "pushed [llength [dict get $jdict commits]] "
				append msg "[plural "commit" [llength [dict get $jdict commits]]] "
				append msg "to [dict get $jdict repository name][formatRef [dict get $jdict ref]]: "
				append msg "[gitio [dict get $jdict compare]]"
				lappend retval $msg
				
				# only show individual commits if they're pushed to the default branch, unless setting enabled
				if {[string equal [dict get $jdict repository default_branch] [string trimleft [formatRef [dict get $jdict ref]] "/"]] || $showNonDefaultCommits == 1} {
					foreach commit [dict get $jdict commits] {
						set msg "[dict get $jdict repository name][formatRef [dict get $jdict ref]] "
						append msg "[string range [dict get $commit id] 0 6] "
						if {[dict exists $commit author username]} {
							append msg "[dict get $commit author username]: "
						} else {
							append msg "[dict get $commit author name]: "
						}
						append msg "[shortenBody [dict get $commit message]]"
						lappend retval $msg
					}
				}
			}
			return $retval
		}
		release {
			set msg "\[[dict get $jdict repository name]\] [dict get $jdict sender login] "
			append msg "[dict get $jdict action] release "
			
			putloglev d * "github.tcl release name: [dict get $jdict release name]"
			if {![string equal -nocase [dict get $jdict release name] "null"]} {
				append msg "[dict get $jdict release name]"
			} else {
				append msg "[dict get $jdict repository name][dict get $jdict release tag_name]"
			}
			
			putloglev d * "github.tcl release body: [dict get $jdict release body]"
			if {![string equal -nocase [dict get $jdict release body] "null"]} {
				append msg ": [shortenBody [dict get $jdict release body]] "
			} else {
				append msg ": "
			}
			
			append msg "[gitio [dict get $jdict release html_url]]"
			return [list $msg]
		}
		status {
			# The status of a commit changed, these are triggered via the API from jenkins, etc
			# The new state. Can be pending, success, failure, or error.
			if {$showPendingStatus != 1 && [string equal [dict get $jdict state] "pending"]} {
				putloglev d * "github.tcl: got \"pending\" status event, ignoring"
				return
			}			
			
			set msg "\[[dict get $jdict repository name]\] [string range [dict get $jdict sha] 0 6] "
		
			if {[dict exists $jdict commit author login]} {
				append msg "[dict get $jdict commit author login]"
			} elseif {[dict exists $jdict commit commit author name]} {
				append msg "[dict get $jdict commit commit author name]"
			} else {
				append msg "[dict get $jdict sender login]"
			}
			
			if {[dict exists $jdict commit commit message] && ![string equal [dict get $jdict commit commit message] "null"]} {
				append msg " ([shortenBody [dict get $jdict commit commit message]])"
			}
			
			append msg ". Status: [string toupper [dict get $jdict state]]"
			
			if {![string equal [dict get $jdict description] "null"]} {
				append msg ": [shortenBody [string trim [dict get $jdict description]]]"
			}
			
			if {[dict exists $jdict target_url] && ![string equal [dict get $jdict target_url] "null"]} {
				append msg ". [dict get $jdict target_url]"
			} elseif {[dict exists $jdict commit commit url]} {
				append msg ". [gitio [dict get $jdict commit commit url]]"
			} else {
				append msg "."
			}
			return [list $msg]
		}
		watch {
			# when someone STARS a repository (not watches it)
			set msg "\[[dict get $jdict repository name]\] [dict get $jdict sender login] "
			append msg "starred [dict get $jdict repository full_name]: "
			append msg "[gitio [dict get $jdict repository html_url]]"
			return [list $msg]
		}
		default {
			putlog "github.tcl: Got unhandled event: $event, ignoring"
		}
	}
	return
}

# further processing of some events that belong together but gets sent separately by github
# FixMe: this could probably grow into a too long line if a lot of different stuff was done to an issue at the same time
proc ::github::delayedSend {event path directChan id len} {
	variable delayedSendIds
	putloglev d * "github.tcl: enter delayedSend: $event <> $path <> $directChan <> $id <> $len"
	putloglev d * "[lindex $delayedSendIds($id) 0] != $len"
	if {[info exists delayedSendIds($id)] && [lindex $delayedSendIds($id) 0] != $len} {
		# there's more coming
		putloglev d * "github.tcl: more coming, returning"
		return
	} else {
		putloglev d * "github.tcl: delayedSend started parsing for id: $id"
		switch -- $event {
			issues {
				# output == $repName $issueNum $action $senderLogin $issueTitle $url [action-specific]
				foreach output [lrange $delayedSendIds($id) 1 end] {
					lassign $output repName issueNum action senderLogin issueTitle url -
					switch -- $action {
						assigned {
							lappend actionAssigned [lindex $output end]
						}
						unassigned {
							lappend actionUnassigned [lindex $output end]
						}
						labeled {
							lappend actionLabeled [lindex $output end]
						}
						unlabeled {
							lappend actionUnlabeled [lindex $output end]
						}
						milestoned {
							lappend actionMilestoned [lindex $output end]
						}
						demilestoned {
							lappend actionDemilestoned [lindex $output end]
						}
						opened {
							lappend mainAction "opened"
						}
						closed {
							lappend mainAction "closed"
						}
						reopened {
							lappend mainAction "reopened"
						}
					}
				}
				if {[info exists mainAction] && [llength $mainAction] > 1} {
					set mainAction [join $mainAction ", "]
				}
				if {![info exists mainAction]} {
					if {[info exists actionAssigned]} {
						set mainAction "assigned"
					} elseif {[info exists actionUnassigned]} {
						set mainAction "unassigned"
					} elseif {[info exists actionLabeled]} {
						set mainAction "labeled"
					} elseif {[info exists actionUnlabeled]} {
						set mainAction "unlabeled"
					} elseif {[info exists actionMilestoned]} {
						set mainAction "milestoned"
					} elseif {[info exists actionDemilestoned]} {
						set mainAction "demilestoned"
					} else {
						# this should never happen
						set mainAction ""
					}
				}
				
				set extraActions ""
				# assigned
				if {[info exists actionAssigned]} {
					if {![string equal $mainAction "assigned"]} {
						append extraActions " and assigned it"
					}
					append extraActions " to [join $actionAssigned ", "]"
				}
				# unassigned
				if {[info exists actionUnassigned]} {
					if {![string equal $mainAction "unassigned"]} {
						if {[info exists actionAssigned]} {
							append extraActions ", unassigned it"
						} else {
							append extraActions " and unassigned it"
						}
					}
					append extraActions " from [join $actionUnassigned ", "]"
				}
				# labeled
				if {[info exists actionLabeled]} {
					if {![string equal $mainAction "labeled"]} {
						if {[info exists actionAssigned] || [info exists actionUnassigned]} {
							append extraActions ", labeled it"
						} else {
							append extraActions " and labeled it"
						}
					}
					append extraActions " with [join $actionLabeled ", "]"
				}
				# unlabeled
				if {[info exists actionUnlabeled]} {
					if {![string equal $mainAction "unlabeled"]} {
						if {[info exists actionAssigned] || [info exists actionUnassigned] || [info exists actionLabeled]} {
							append extraActions ", unlabeled it"
						} else {
							append extraActions " and unlabeled it"
						}
					}
					append extraActions " with [join $actionUnlabeled ", "]"
				}
				# milestoned
				if {[info exists actionMilestoned]} {
					if {![string equal $mainAction "milestoned"]} {
						if {[info exists actionAssigned] || [info exists actionUnassigned] || [info exists actionLabeled] || [info exists actionUnlabeled]} {
							append extraActions ", milestoned it"
						} else {
							append extraActions " and milestoned it"
						}
					}
					append extraActions " with [join $actionMilestoned ", "]"
				}
				# demilestoned
				if {[info exists actionDemilestoned]} {
					if {![string equal $mainAction "demilestoned"]} {
						if {[info exists actionAssigned] || [info exists actionUnassigned] || [info exists actionLabeled] || [info exists actionUnlabeled] || [info exists actionMilestoned]} {
							append extraActions ", demilestoned it"
						} else {
							append extraActions " and demilestoned it"
						}
					}
				}
				set msg "\[${repName}\] $senderLogin $mainAction issue #${issueNum} (${issueTitle})${extraActions} [gitio $url]"
				putloglev d * "github.tcl: msg: $msg"
				outputMsg $event $path [list $msg] $directChan
				unset delayedSendIds($id)
				return
			}
			default {
				putlog "github.tcl error: delayedSend for unhandled event: $event"
				return
			}
		}
	}
}

# main parsing proc that gets called after a new payload is received
proc ::github::processData {event path data {directChan ""}} {
	variable delayedSendIds
	
	set delayedSend 0; # this is upvar'd from parseJson
	set output [parseJson $event $data]
	
	if {$delayedSend} {
		switch -- $event {
			issues {
				# output == $repName $issueNum $action $senderLogin $issueTitle $url [action-specific]
				lassign $output repName issueNum -
				set id "$repName,$issueNum"
			}
			default {
				putlog "github.tcl error: processData for unhandled event: $event"
				return
			}
		}

		if {![info exists delayedSendIds($id)]} {
			set delayedSendIds($id) 0
		}
		lappend delayedSendIds($id) $output
		set delayedSendIds($id) [lreplace $delayedSendIds($id) 0 0 [llength $delayedSendIds($id)]]
		putloglev d * "github.tcl: calling utimer with $event <> $path <> $directChan <> $id <> [llength $delayedSendIds($id)]"
		# FixMe: I don't like this, if there's any error in the proc then delayedSendIds will grow forever. Maybe add some short proc
		# in between that's responsible for calling delayedSend and unsetting delayedSendIds
		utimer 10 [list ::github::delayedSend $event $path $directChan $id [llength $delayedSendIds($id)]]
		return
	}
	outputMsg $event $path $output $directChan
	return
}

# output to IRC
proc ::github::outputMsg {event path output {directChan ""}} {
	variable settings
	variable privmsgTargMax
	
	if {![string equal $output ""]} {
		if {![string equal $directChan ""]} {
			foreach line $output {
				putserv "PRIVMSG $directChan :$line"
			}
			return
		}
		
		# find channels to output to and stick them in $targets
		foreach name [dict keys $settings] {
			if {[dict exists $settings $name path] && [string equal [dict get $settings $name path] $path]} {
				foreach c [channels] {
					if {[dict exists $settings $name channels $c]} {
						foreach {setChan setEvents} [dict get $settings $name channels] {
							if {([ircstreql $c $setChan]) && ([lsearch -exact $setEvents $event] != -1)} {
								lappend targets $c
							}
						}
					}
				}
			}
		}
		
		# output to the appropriate channels
		foreach line $output {
			if {[info exists targets]} {
				foreach target $targets {
					if {[llength [lappend outChans $target]] == $privmsgTargMax} {
						putserv "PRIVMSG [join $outChans ","] :$line"
						unset outChans
					}
					if {[info exists outChans]} {
						putserv "PRIVMSG [join $outChans ","] :$line"
					}
				}
			} else {
				putloglev d * "github.tcl processed \"${event}\" event but found no channels to output to: $line"
			}
		}
	}
	return
}

proc ::github::cleanup {sock} {
	variable state
	
	set state [dict remove $state $sock]	
	catch { chan flush $sock }
	catch { chan close $sock }
	return
}

proc ::github::sendError {sock code errmsg} {
	variable state
	array set errors {
		400 {Bad Request}
		401 {Unauthorized}
		403 {Forbidden}
		404 {Not Found}
		411 {Length Required}
		413 {Payload Too Large}
		431 {Request Header Fields Too Large}
		500 {Internal Server Error}
	}

	set message "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">"
	append message "<html><head><title>$code $errors($code)</title></head>"
	append message "<body><h1>$code $errors($code)</h1></body></html>"
	chan configure $sock -blocking 0 -buffering line -translation {auto crlf}
	chan puts $sock "HTTP/1.1 $code $errors($code)"
	chan puts $sock "Connection: Close"
	chan puts $sock "Content-Type: text/html"
	chan puts $sock "Content-Length: [string bytelength $message]"
	if {$code == 401} {
		chan puts $sock "WWW-Authenticate: Basic realm=\"Authenticate\""
	}
	chan puts $sock ""
	chan puts $sock $message
	cleanup $sock
	putlog "github.tcl HTTP Error: $code $errors($code): $errmsg"
	return
}

proc ::github::acceptConnection {sock addr port} {
	variable state

	dict set state $sock timestamp [clock seconds]
	putloglev d * "github.tcl: new connection from $addr"
	chan configure $sock -blocking 0 -buffering line -translation {auto crlf}
	chan event $sock readable [list ::github::httpHeader $sock]
	return
}

proc ::github::httpHeader {sock} {
	variable state
	variable settings
	variable payloadLimit
	variable headerLimit

	# FixMe: make sure this doesn't read endlessly until it gets a newline
	set readBytes [gets $sock line]
	putloglev 5 * "github.tcl <- $line"
	if {$readBytes > 0} { set readData 1 } else { set readData 0 }

	if {![dict exists $state $sock state]} {
		if {[regexp -- {^(POST) ([^?]+)\??([^ ]*) HTTP/1.(0|1)$} $line - method path query]} {

			foreach name [dict keys $settings] {
				if {![string equal $name "linker"] && [string equal [dict get $settings $name path] $path]} {
					dict set state $sock name $name
					dict set state $sock method $method
					dict set state $sock path $path
					dict set state $sock query $query
					dict set state $sock state "mime"
					break
				}
			}
			if {![dict exists $state $sock name]} {
				sendError $sock 403 "No such path: $path"
				return
			}
		} else {
			sendError $sock 403 "Bad request: $line"
			return
		}
	}

	switch -- "$readData,[dict get $state $sock state]" {
		1,mime {
			# this is a header line
			if {[regexp -- {^([^:]+):[   ]*(.*)} $line - key value]} {
				dict set state $sock mime [string tolower $key] $value
				if {[string bytelength [dict get $state $sock mime]] >= $headerLimit} {
					sendError $sock 431 "Header section above header limit"
					return
				}
			}
		}
		0,mime {
			# we've received all headers, check that everything is ok
			putloglev 6 * "mime: [dict get $state $sock mime]"
			set basicAuth "[dict get $settings [dict get $state $sock name] username]:[dict get $settings [dict get $state $sock name] password]"
			if {![dict exists $state $sock mime authorization]} {
				sendError $sock 401 "Authorization header not sent"
				return
			} elseif {![string equal [dict get $state $sock mime authorization] "Basic [::base64::encode -maxlen 0 $basicAuth]"]} {
				sendError $sock 401 "Authorization error, bad username or password"
				return
			} elseif {![dict exists $state $sock mime content-length]} {
				sendError $sock 411 "Content-Length missing"
				return
			} elseif {![string is integer [dict get $state $sock mime content-length]]} {
				sendError $sock 400 "Malformed Content-Length"
				return
			} elseif {[dict get $state $sock mime content-length] >= $payloadLimit} {
				sendError $sock 413 "Content-Length above payload limit"
				return
			} elseif {![dict exists $state $sock mime x-github-event]} {
				sendError $sock 400 "X-GitHub-Event header missing"
				return
			} elseif {![dict exists $state $sock mime x-github-delivery]} {
				sendError $sock 400 "X-GitHub-Delivery header missing"
				return
			} elseif {![dict exists $state $sock mime x-hub-signature]} {
				sendError $sock 400 "X-Hub-Signature header missing"
				return
			}
			# everything is fine, go ahead and receive the payload
			putloglev d * "github.tcl: end of header, switching to query"
			dict set state $sock remaining [dict get $state $sock mime content-length]
			putloglev 7 * "content-length: [dict get $state $sock mime content-length]"
			putloglev 7 * "remaining: [dict get $state $sock remaining]"
			dict set state $sock state query
			chan configure $sock -blocking 0 -buffering full -translation binary
			chan event $sock readable [list ::github::httpQuery $sock]
			putloglev 7 * "done switching"
		}
		default {
			if {[eof $sock]} {
				sendError $sock 500 "Unexpected EOF on request"
			} else {
				sendError $sock 500 "Unhandled state <${readData},[dict get $state $sock state]>"
			}
			
		}
	}
	return
}

proc ::github::httpQuery {sock} {
	variable state
	variable settings
	variable storeJson

	set this [read $sock]
	set remaining [expr {[dict get $state $sock remaining] - [string bytelength $this]}]
	if {$remaining < 0} { set remaining 0 }
	dict set state $sock remaining $remaining
	if {![dict exists $state $sock data]} {
		dict set state $sock data $this
	} else {
		dict set state $sock data "[dict get $state $sock data]$this"
	}
	putloglev d * "github.tcl: Got [string bytelength $this] bytes. Remaining: [dict get $state $sock remaining]."
	if {[eof $sock]} {
		sendError $sock 500 "Unexpected EOF on request"
		return
	}
	if {[dict get $state $sock remaining] <= 0} {
		putloglev d * "github.tcl: Complete. Payload: [dict get $state $sock data]"
		# payload sent, save some things we need
		set data [dict get $state $sock data]
		set github(event) [dict get $state $sock mime x-github-event]
		set github(delivery) [dict get $state $sock mime x-github-delivery]
		set github(signature) [dict get $state $sock mime x-hub-signature]
		set github(path) [dict get $state $sock path]
		set sharedSecret [dict get $settings [dict get $state $sock name] secret]
	
		# send the response and remove the state array
		httpResponse $sock
		
		# attempt to fix encoding
		set data [encoding convertfrom utf-8 $data]
		
		putloglev 7 * "github.tcl github event: $github(event)"
		putloglev 7 * "got : $github(signature)"
		putloglev 7 * "want: sha1=[::sha1::hmac -hex -key $sharedSecret [encoding convertto utf-8 $data]]"
		
		if {$storeJson} {
			putloglev d * "Saving JSON payload to scripts/jsonStore"
			set fd [open scripts/jsonStore a+]
			chan configure $fd -translation binary
			chan puts $fd "time: [clock format [clock seconds]]"
			chan puts $fd "event: $github(event)"
			chan puts $fd "delivery: $github(delivery)"
			chan puts $fd "signature: $github(signature)"
			chan puts $fd "$data\n"
			chan close $fd
		}

		if {([string equal "sha1=[::sha1::hmac -hex -key $sharedSecret [encoding convertto utf-8 $data]]" $github(signature)]) && (![string equal $github(event) ""])} {
			processData $github(event) $github(path) $data
		}
	}
	return
}

proc ::github::httpResponse {sock} {
	variable state

	putloglev d * "github.tcl: Sent httpResponse"
	chan configure $sock -blocking 0 -buffering line -translation {auto crlf}
	chan puts $sock "HTTP/1.1 200 OK"
	chan puts $sock "Connection: Close"
	chan puts $sock "Content-Length: 0"
	chan puts $sock ""
	chan flush $sock
	chan close $sock

	set state [dict remove $state $sock]
	return
}

proc ::github::timeout {args} {
	variable state
	
	if {[info exists state]} {
		foreach sock [dict keys $state] {
			if {([clock seconds] - [dict get $state $sock timestamp]) > 120} {
				cleanup $sock
			}
		}
	}
	return
}

# http://wiki.tcl.tk/29163
proc ::github::randIntUrandom { min max } {
    set randDev [open /dev/urandom rb]
    set random [read $randDev 8]
    binary scan $random H16 random
    set random [expr {([scan $random %x] % (($max-$min) + 1) + $min)}]
    close $randDev
    return $random
}

# http://wiki.tcl.tk/1549
proc ::github::randIntRand {min max} {
    return [expr {int(rand()*($max-$min+1)+$min)}]
}

# http://wiki.tcl.tk/3757
# slightly modified
#
# I have no idea if this is sufficiently random to be considered
# cryptographically secure. For the purposes of this script I feel it should be
# good enough. If you're going to use this for some more critical purpose I
# suggest have a cryptography expert look at it first.
proc ::github::randomString {min {max -1} {chars "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"}} {	
	if {$max == -1} { set max $min }
	# Use /dev/urandom if we can, if it doesn't exist fall back to Tcl rand()
	if {[file exists /dev/urandom]} {
		set rand "randIntUrandom"
	} else {
		set rand "randIntRand"
	}
	set range [expr {[string length $chars] - 1}]
	set stringLength [$rand $min $max]
	
	for {set i 0} {$i < $stringLength} {incr i} {
		append randomString [string index $chars [$rand 0 $range]]
	}
	return $randomString
}

proc ::github::settingsFileOk {} {
	variable settingsFile
	if {![file exists $settingsFile]} {
		if {[catch {open $settingsFile w} fd]} {
			putlog "github.tcl: Error: failed to create settings file: $fd"
			# FixMe: make this unload the script or something?
			return 0
		} else {
			chan close $fd
		}
	} elseif {[file isdirectory $settingsFile]} {
		putlog "github.tcl Error: settings file is a directory."
		return 0
	} elseif {![file writable $settingsFile]} {
		putlog "github.tcl Error: settings file is not writable."
		return 0
	} elseif {![file readable $settingsFile]} {
		putlog "github.tcl Error: settings file is not readable."
		return 0
	}
	return 1
}

proc ::github::saveSettings {} {
	variable settingsFile
	variable settings
	
	if {[settingsFileOk]} {
		if {[catch {open $settingsFile w} fd]} {
			putlog "github.tcl Error: failed to open settings file: $fd"
			# FixMe: make this unload the script or something?
			return
		} else {
			chan puts $fd [dict get $settings]
			chan close $fd
		}
	}
	return
}

proc ::github::loadSettings {} {
	variable settingsFile
	variable settings
	if {[settingsFileOk]} {
		if {[catch {open $settingsFile r} fd]} {
			putlog "github.tcl Error: failed to open settings file: $fd"
			# FixMe: make this unload the script or something?
			return
		} else {
			set data [read $fd]
			chan close $fd
			if {[catch {dict keys $data}]} {
				putlog "github.tcl Error: failed to parse settings file."
				# FixMe: make this unload the script or something?
				# FixMe: add more validation?
				return
			}
			set settings $data
		}
	}
	return
}

proc ::github::dccCommand {handle idx text} {
	variable settings
	variable port
	variable validEvents
	set args [split $text]
	# .github add <name> <path>
	# .github delete <name>
	# .github rename <old name> <new name>
	#
	# .github set <name> <username/password/secret/path/channel> <value> [value...]
	#
	# .github info [name/linker]
	#
	# .github
	# .github help
	
	switch -nocase -- [lindex $args 0] {
		add {
			# .github add <name> <path> [username] [password] [secret]
			lassign $args - name path username password secret
			set name [string tolower $name]
			if {[llength $args] < 3} {
				putdcc $idx {Usage: .github add <name> <path> [username] [password] [secret]}
				return
			} elseif {[dict exists $settings $name]} {
				putdcc $idx "Error: \"[lindex $args 1]\" already exists."
				return
			} elseif {![string equal [string index $path 0] "/"]} {
				putdcc $idx "Invalid path \"${path}\""
				return
			} else {
				set path [string trimright $path "/"]
				dict set settings $name path $path
				putdcc $idx "Added $name with path $path"
				if {[string equal $username ""]} {
					set username [randomString 20 25]
					putdcc $idx "Username: $username (automatically generated)"
				} else {
					putdcc $idx "Username: $username"
				}
				if {[string equal $password ""]} {
					set password [randomString 20 25]
					putdcc $idx "Password: $password (automatically generated)"
				} else {
					putdcc $idx "Password $password"
				}
				if {[string equal $secret ""]} {
					set secret [randomString 20 25]
					putdcc $idx "GitHub webhook secret: $secret (automatically generated)"
				} else {
					putdcc $idx "GitHub webhook secret: $secret"
				}
				dict set settings $name username $username
				dict set settings $name password $password
				dict set settings $name secret $secret
				dict set settings $name channels ""
				putdcc $idx "GitHub webhook URL: http://${username}:${password}@<your.host.or.ip>:${port}${path}"
				saveSettings
				return 1
			}
		}
		remove -
		del -
		delete {
			# .github delete <name>
			set name [string tolower [lindex $args 1]]
			if {[llength $args] != 2} {
				putdcc $idx "Usage: .github delete <name>"
				return
			} elseif {![dict exists $settings $name]} {
				putdcc $idx "Error: no such name \"${name}\""
			} else {
				set settings [dict remove $settings $name]
				saveSettings
				putdcc $idx "Deleted $name"
				return 1
			}
		}
		rename {
			# .github rename <old name> <new name>
			lassign $args - oldName newName
			set oldName [string tolower $oldName]
			set newName [string tolower $newName]
			if {[llength $args] != 3} {
				putdcc $idx "Usage: .github rename <old name> <new name>"
				return
			} elseif {![dict exists $settings $oldName]} {
				putdcc $idx "Error: no such name \"${oldName}\""
				return
			} elseif {[dict exists $settings $newName]} {
				putdcc $idx "Error: \"${newName}\" already exists"
				return
			} else {
				dict set settings $newName [dict get $settings $oldName]
				set settings [dict remove $settings $oldName]
				saveSettings
				putdcc $idx "Renamed $oldName to $newName"
				return 1
			}
		}
		set {
			# .github set <name> <path/username/password/secret/channel> <value> [value ...]
			# .github set <name> channel <#channel> <event> \[event...\]
			lassign $args - name subCmd value
			set name [string tolower $name]
			set subCmd [string tolower $subCmd]
			if {[llength $args] < 4} {
				putdcc $idx "Usage: .github set <name> <path/username/password/secret/channel> <value> \[value ...\]"
				return
			} elseif {![dict exists $settings $name]} {
				putdcc $idx "Error: no such name \"${name}\""
				return
			}

			switch -- $subCmd {
				path {
					if {![string equal [string index $value 0] "/"]} {
						putdcc $idx "Invalid path \"${value}\""
						return
					} else {
						set value [string trimright $value "/"]
						dict set settings $name path $value
						saveSettings
						putdcc $idx "Path for $name set to $value"
						return 1
					}
				}
				username -
				login {
					dict set settings $name username $value
					saveSettings
					putdcc $idx "Username for $name set to $value"
					return 1
				}
				pass -
				password {
					dict set settings $name password $value
					saveSettings
					putdcc $idx "Password for $name set to $value"
					return 1
				}
				secret {
					dict set settings $name secret $value
					saveSettings
					putdcc $idx "Secret for $name set to $value"
					return 1
				}
				channel {
					lassign $args - name - chan
					set name [string tolower $name]
					set events [split [string tolower [join [lrange $args 4 end]]]]

					if {[llength $args] < 5} {
						putdcc $idx "Usage: .github set <name> channel <#channel> <event> \[event...\]"
						return
					} elseif {![dict exists $settings $name]} {
						putdcc $idx "Error: no such name \"${name}\""
						return
					} elseif {![validchan $chan]} {
						putdcc $idx "Error: $chan is not a valid channel"
						return
					}
					foreach event $events {
						if {[string equal $event "all"] && [llength $args] > 5} {
							putdcc $idx "Error: mixing \"all\" with other events"
							return
						} elseif {[lsearch -exact [concat all $validEvents] $event] == -1} {
							putdcc $idx "Error: $event is not a valid event. Event has to be \"all\" to mean all events or any number of the following:"
							putdcc $idx "[join $validEvents]"
							return
						}
					}
					if {[llength $args] == 5 && [string equal -nocase [lindex $events 0] "all"]} {
						dict set settings $name channels $chan $validEvents
					} else {
						dict set settings $name channels $chan [lrange $events 0 end]
					}
					saveSettings
					putdcc $idx "Channel \"${chan}\" set to output the following GitHub events from \"${name}\":"
					putdcc $idx [join [dict get $settings $name channels $chan]]
					return 1
				}
				default {
					putdcc $idx "Error: unknown option \"${subCmd}\": must be path, username, password, secret or channel"
					return
				}
			}
		}
		unset {
			# .github unset <name> channel <channel>
			lassign $args - name subCmd chan
			set name [string tolower $name]
			set subCmd [string tolower $subCmd]
			if {[llength $args] != 4} {
				putdcc $idx "Usage: .github unset <name> channel <channel>"
				return
			} elseif {![dict exists $settings $name]} {
				putdcc $idx "Error: no such name \"${name}\""
				return
			} elseif {![string equal $subCmd "channel"]} {
				putdcc $idx "Usage: .github unset <name> channel <channel>"
				return
			} elseif {![dict exists $settings $name channels $chan]} {
				putdcc $idx "Error: no such channel \"${chan}\""
				return
			} else {
				set settings [dict remove $settings $name channels $chan]
				saveSettings
				putdcc $idx "Removed channel $chan"
				return 1
			}
		}
		list -
		info {
			# .github info [name/linker]
			lassign $args - name
			set name [string tolower $name]
			if {[llength $args] > 2} {
				putdcc $idx "Usage: .github info \[name\]"
				return
			} elseif {[llength $args] == 2 && ![dict exists $settings $name]} {
				putdcc $idx "Error: no such name \"${name}\""
				return
			}
			foreach n [dict keys $settings] {
				if {([llength $args] == 2) && (![string equal $name $n])} { continue }
				if {[string equal $n "linker"]} { continue }
				putdcc $idx "Settings for \"${n}\":"
				putdcc $idx "  Path: [dict get $settings $n path]"
				putdcc $idx "  Username: [dict get $settings $n username]"
				putdcc $idx "  Password: [dict get $settings $n password]"
				putdcc $idx "  GitHub secret: [dict get $settings $n secret]"
				
				set msg "  GitHub webhook URL: http://"
				append msg "[dict get $settings $n username]:"
				append msg "[dict get $settings $n password]"
				append msg "@your.ip.or.hostname:${port}"
				append msg "[dict get $settings $n path]"
				putdcc $idx $msg

				putdcc $idx "  Configured channels:"
				foreach {chan events} [dict get $settings $n channels] {
					set disabledEvents [lmap x $validEvents { expr {[lsearch -exact $events $x] == -1 ? $x : [continue] } }]
					putdcc $idx "    Channel: $chan"
					if {[string equal $disabledEvents ""]} {
						putdcc $idx "      Enabled events: [join $events]"
						putdcc $idx "      No disabled events."
					} elseif {[string equal $events ""]} {
						putdcc $idx "      No enabled events."
						putdcc $idx "      Disabled events: [join $disabledEvents]"
					} else {
						putdcc $idx "      Enabled events: [join $events]"
						putdcc $idx "      Disabled events: [join $disabledEvents]"
					}
				}
			}
			putdcc $idx "Linker settings:"
			foreach {chan path} [dict get $settings linker] {
				putdcc $idx "  $chan: $path"
			}
			return 1
		}
		linker {
			lassign $args - subCmd chan repo
			
			switch -- [string tolower $subCmd] {
				set {
					if {[llength $args] != 4} {
						putdcc $idx "Usage: .github linker set <channel> <repository>"
						return
					} elseif {![validchan $chan]} {
						putdcc $idx "Error: $chan is not a valid channel"
						return
					} elseif {![regexp -- {^/[a-zA-Z0-9]+/[a-zA-Z0-9]+$} $repo]} { ;# FixMe: make sure this is correct
						putdcc $idx "Error: repository has to be a valid GitHub repository in the form of \"/username/repository\""
						return
					} else {
						dict set settings linker $chan $repo
						saveSettings
						putdcc $idx "Set repository \"${repo}\" for $chan"
						return 1
					}
				}
				unset {
					if {[llength $args] != 3} {
						putdcc $idx "Usage: .github linker set <channel>"
						return
					} elseif {![dict exists $settings linker $chan]} {
						putdcc $idx "No such channel: \"${chan}\""
					} else {
						set settings [dict remove $settings linker $chan]
						saveSettings
						putdcc $idx "Removed repository for $chan"
						return 1
					}
				}
				default {
					putdcc $idx "Error: unknown subcommand \"${subCmd}\""
					return
				}
			}
			return
		}
		help -
		default {
			# .github help
			# FixMe: make help more clear?
			putdcc $idx {.github add <name> <path> [username] [password] [secret]}
			putdcc $idx {.github delete <name>}
			putdcc $idx {.github rename <old name> <new name>}
			putdcc $idx {.github set <name> <path/username/password/secret/channel> <value> [value ...]}
			putdcc $idx {.github unset <name> channel <channel>}
			putdcc $idx {.github list [name/linker]}
			putdcc $idx {.github linker set <channel> <repository>}
			putdcc $idx {.github linker unset <channel>}
			putdcc $idx {.github help}
			return 1
		}
	}
	return 1
}

namespace eval ::github {
	if {![info exists initDone]} {
		socket -server ::github::acceptConnection $port
		set initDone 1
	}

	::http::register https 443 [list ::tls::socket -require 0 -request 1]

	# Make sure we're using UTF-8
	encoding system utf-8
	
	bind time - "*" ::github::timeout
	bind dcc $dccFlags github ::github::dccCommand
	bind pubm - "*" ::github::issueLinker

	loadSettings
	
	putlog "Loaded github.tcl v0.2 by Pixelz"
}