#! /bin/bash
#
# halt-all.sh
#
# Stop all LXC containers.
#
# See also rc.local.shutdown

# This just "cleanly" halts all of the LXC containers.
lxc-ls --active | xargs -r -n1 lxc-stop -n
