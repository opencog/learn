#! /bin/bash
#
# run-tmux.sh
#
# Run tmux with byobu to multiplex multiple terminals.
# This just opens a bunch of terminals, and nothing more.
# The terminal names are just a convention that seems handy
# for a generic workflow.
#
# Use F3 and F4 to switch between terminals. `man byobu` for more.
#
#------------------

# Work around an lxc-attach bug.
if [[ `tty` == "not a tty" ]]
then
	script -c $0 /dev/null
	exit 0
fi

# Load config parameters
if [ -z $MASTER_CONFIG_FILE ]; then
	echo "MASTER_CONFIG_FILE not defined!"
	exit -1
fi

if [ -r $MASTER_CONFIG_FILE ]; then
	. $MASTER_CONFIG_FILE
else
	echo "Cannot find master configuration file!"
	exit -1
fi

# Use byobu so that the scroll bars actually work
byobu new-session -d -n 'cntl' \
	'echo -e "\nControl shell; you might want to run top here.\n"; $SHELL'

byobu new-window -n 'cogsrv' \
	'echo -e "\nGuile shell; e.g. run guile -l cogserver.scm here. \n"; $SHELL'

# Telnet window
tmux new-window -n 'telnet' \
	'echo -e "\nTelnet shell; e.g. run rlwrap telnet $HOSTNAME $PORT\n"; $SHELL'

# Parse
tmux new-window -n 'submit' \
	'echo -e "\nYou might want to run ./pair-submit.sh here.\n"; $SHELL'

# Code
tmux new-window -n 'code' 'echo -e "\nCoding shell.\n"; $SHELL'

# Spare
tmux new-window -n 'spare' 'echo -e "\nSpare-use shell.\n"; $SHELL'

# Fix the annoying byobu display
echo "tmux_left=\"session\"" > $HOME/.byobu/status
echo "tmux_right=\"load_average disk_io date time\"" >> $HOME/.byobu/status
tmux attach
