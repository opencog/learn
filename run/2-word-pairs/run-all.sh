#! /bin/bash
#
# run-all.sh
#
# Run a fully-automated word-pair counting pipeline. This will run the
# conventional tmux/byobu panel, but will fully automate. This allows
# progress monitoring.
#
# Use F3 and F4 to switch to the other terminals.
#
# ----------------------

# Work around an lxc-attach bug.
if [[ `tty` == "not a tty" ]]
then
	script -c $0 /dev/null
	exit 0
fi

# Load master config parameters
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

# ----------------------
# Pair-counting config
if ! [ -z ${PAIR_CONF_FILE} ] && [ -r ${PAIR_CONF_FILE} ]; then
	source ${PAIR_CONF_FILE}
else
	echo "Cannot find pair-counting configuration file!"
	exit -1
fi

# ==================================
# Use byobu so that the scroll bars actually work.
byobu new-session -d -s 'auto-pair-count' -n 'cntl' 'top; $SHELL'

# Start cogserver. When it exits, start marginal computation.
byobu new-window -n 'cogsrv' 'nice guile -l ${COMMON_DIR}/cogserver-pair.scm ; ./compute-marginals.sh ; $SHELL'

# Wait for the CogServer to initialize.
# netcat -z returns 1 upon connection.
while ! nc -z $HOSTNAME $PORT ; do
   echo "Wating for CogServer at $HOSTNAME $PORT ..."
   sleep 1
done
echo "Found CogServer at $HOSTNAME $PORT"

# Telnet window
tmux new-window -n 'telnet' 'rlwrap telnet $HOSTNAME $PORT; $SHELL'

# Redefine handler so that the CogServer exits when pairs are done.
echo -e "(define (finish-pair-submit) (exit-server))\n.\n." | nc $HOSTNAME $PORT >> /dev/null

# echo -e "(block-until-idle 0.01)\n.\n." | nc $HOSTNAME $PORT >> /dev/null
echo -e "(wait-gate startup-gate)\n.\n." | nc $HOSTNAME $PORT >> /dev/null

# Batch-process the corpus.
tmux new-window -n 'submit' './pair-submit.sh; $SHELL'

# Spare
tmux new-window -n 'spare' 'echo -e "\nSpare-use shell.\n"; $SHELL'

# Fix the annoying byobu display
echo "tmux_left=\"session\"" > $HOME/.byobu/status
echo "tmux_right=\"load_average disk_io date time\"" >> $HOME/.byobu/status
tmux attach

#echo Done processing word-pairs
## ------------------------
