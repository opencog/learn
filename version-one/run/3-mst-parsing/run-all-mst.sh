#! /bin/bash
#
# run-all-mst.sh
#
# Run a fully-automated MST parsing session.
# This will start the same byobu terminal session as the manual mode,
# so that it can be used for monitoring. However, all the needed work
# will auto-launch.
#
# Use F3 and F4 to switch to the other terminals.
#

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
	env |grep CONF
	exit -1
fi

if [ -r ${MST_CONF_FILE} ]; then
	source ${MST_CONF_FILE}
else
	echo "Cannot find MST configuration file!"
	env |grep CONF
	exit -1
fi

# Use byobu so that the scroll bars actually work
byobu new-session -d -s 'mst-count-auto' -n 'cntl' 'top; $SHELL'

byobu new-window -n 'cogsrv' ' \
	nice guile -l ${COMMON_DIR}/cogserver-mst.scm ; \
	nice ./compute-mst-marginals.sh ; \
	$SHELL'

# Wait for the CogServer to initialize.
# netcat -z returns 1 upon connection.
while ! nc -z $HOSTNAME $PORT ; do
	echo "Wating for cogserver at $HOSTNAME $PORT ..."
	sleep 1
done
echo "Found CogServer at $HOSTNAME $PORT"

# Telnet window
tmux new-window -n 'telnet' 'rlwrap telnet $HOSTNAME $PORT; $SHELL'

# Redefine handler so that the CogServer exits when MST is done.
echo -e "(define (finish-mst-submit) (exit-server))\n.\n." | nc $HOSTNAME $PORT >> /dev/null

# The gate below is only on the base CogServer.
# The mst-gate is opened, after all pairs are loaded.
echo -e "(wait-gate startup-gate)\n.\n." | nc $HOSTNAME $PORT >> /dev/null

# Parse
tmux new-window -n 'submit' './mst-submit.sh; $SHELL'

# Spare
tmux new-window -n 'spare' 'echo -e "\nSpare-use shell.\n"; $SHELL'

# Fix the annoying byobu display
echo "tmux_left=\"session\"" > $HOME/.byobu/status
echo "tmux_right=\"load_average disk_io date time\"" >> $HOME/.byobu/status
tmux attach
