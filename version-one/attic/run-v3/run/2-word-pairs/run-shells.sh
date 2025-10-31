#! /bin/bash
#
# run-shells.sh
#
# Run tmux with byobu to multiplex multiple terminals; start the
# CogServer in one terminal, and suggest which processes to run
# in others.
#
# Use F3 and F4 to switch to the other terminals.
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

if [ -r ${PAIR_CONF_FILE} ]; then
	. ${PAIR_CONF_FILE}
else
	echo "Cannot find pair-counting configuration file!"
	exit -1
fi

# Use byobu so that the scroll bars actually work
byobu new-session -d -n 'cntl' \
	'echo -e "\nControl shell; you might want to run 'top' here.\n"; $SHELL'

byobu new-window -n 'cogsrv' 'nice guile -l ${COMMON_DIR}/cogserver.scm ;   $SHELL'
sleep 2;

# Telnet window
tmux new-window -n 'telnet' 'rlwrap telnet $HOSTNAME $PORT; $SHELL'

# Parse
tmux new-window -n 'submit' \
	'echo -e "\nYou might want to run ./pair-submit.sh here.\n"; $SHELL'

# Spare
tmux new-window -n 'spare' 'echo -e "\nSpare-use shell.\n"; $SHELL'

# Fix the annoying byobu display
echo "tmux_left=\"session\"" > $HOME/.byobu/status
echo "tmux_right=\"load_average disk_io date time\"" >> $HOME/.byobu/status
tmux attach
