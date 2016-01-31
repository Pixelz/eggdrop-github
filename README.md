# eggdrop-github
GitHub Webhook script for Eggdrop

This script receives GitHub webhook events. It creates a partial HTTPd and waits for events getting pushed to it from GitHub. You need to have access to the webhook settings of the repository you wish to get notifications from on GitHub. Some settings which are static in nature can be found at the top portion of the script.

The majority of the settings for this script are available via the ".github" command from the partyline.
