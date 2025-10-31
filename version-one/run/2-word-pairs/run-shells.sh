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
	source $MASTER_CONFIG_FILE
else
	echo "Cannot find master configuration file!"
	exit -1
fi

if [ -r ${PAIR_CONF_FILE} ]; then
	source ${PAIR_CONF_FILE}
else
	echo "Cannot find pair-counting configuration file!"
	exit -1
fi

# Use byobu so that the scroll bars actually work.
byobu new-session -d -s 'pair-count' -n 'cntl' \
	'echo -e "\nControl shell; you might want to run 'top' here.\n"; $SHELL'

byobu new-window -n 'cogsrv' 'nice guile -l ${COMMON_DIR}/cogserver-pair.scm ; $SHELL'

# Wait for the CogServer to initialize.
# netcat -z returns 1 upon connection.
while ! nc -z $HOSTNAME $PORT ; do
	echo "Wating for cogserver at $HOSTNAME $PORT ..."
	sleep 2
done
echo "Found CogServer at $HOSTNAME $PORT"

# echo -e "(block-until-idle 0.01)\n.\n." | nc $HOSTNAME $PORT >> /dev/null
echo -e "(wait-gate startup-gate)\n.\n." | nc $HOSTNAME $PORT >> /dev/null

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
