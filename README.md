# github.tcl
GitHub Webhook script for [the Eggdrop IRC bot](http://www.eggheads.org/).

## About
This scripts recieves GitHub webhook events and relays them to IRC channels. It creates a partial HTTPd and waits for events getting pushed to it from GitHub. You need to have access to the webhook settings of the repository you wish to get notifications from on GitHub. Some settings which are static in nature can be found at the top portion of the script.

The majority of the settings for this script are available via the `.github` command from the partyline.

## Features
- Receives events from GitHub via built-in HTTPd
- Supports many event types
- URL shortening via [git.io](https://git.io/)
- Multiple repositories
- Multiple channels
- Links issues & pull requests when typed in a channel like `#123`

## Prerequisites
- [Eggdrop](http://www.eggheads.org/) 1.6.21 or above
- Tcl 8.6 or above
- [Tcllib](http://core.tcl.tk/tcllib)
- [TLS](http://tls.sourceforge.net/)

## Installation
- Put github.tcl in your Eggdrop `scripts/` directory
- Add `source scripts/github.tcl` at the bottom of your Eggdrop config file
- `.rehash` (or restart) Eggdrop
- Add your repository to the bot. From the partyline (DCC chat): `.github add SomeName /github/path`. The name can be anything you like. The path can be anything you like, but I recommend setting it to the real GitHub path. For this repository for instance, the path would be `/Pixelz/eggdrop-github`. This will generate a secret and webhook URL that you need to paste into the repository settings in GitHub. Replace `your.ip.or.hostname` in the URL with your actual IP-address or hostname.
- Set a channel to output all events to: `.github set SomeName channel #YourChannel all`
- Set another channel to only output some events to: `.github set SomeName channel #YourChannel push release`
- Set the linker repository for a channel. This will make the bot link issues etc when typed in the channel: `.github linker set #YourChannel /github/path`. This path needs to be a valid GitHub path. For this repository, the path would be `/Pixelz/eggdrop-github`.
