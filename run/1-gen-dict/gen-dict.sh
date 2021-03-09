#! /bin/bash
#
# gen-dict.sh
#
# Automated artificial dictionary generation. Configurable parameters
# are located in `run-config/1-dict-conf.scm`. Edit that file as desired.
#
# ---------

if [ -r $MASTER_CONFIG_FILE ]; then
	. $MASTER_CONFIG_FILE
else
	echo "Cannot find master configuration file!"
	exit -1
fi

# Generate a dictionary. The return string is either an error message,
# or its the configured directory.
RET_STR=`./gen-dict.scm $CONFIG_DIR/$DICT_CONF $DICT_DIR`

if [ $? -ne 0 ]; then
	echo $RET_STR
	exit -1
fi

exit 0
