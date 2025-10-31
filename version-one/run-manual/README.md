
Management scripts that require manual install
==============================================
These scripts need to be installed into the system, but you have to
install them by hand.

Shutdown scripts
----------------
Shutdown scripts. Once these are correctly configured and installed,
they will be invoked automatically by the OS during a power outage,
or during a normal shutdown. They attempt to properly halt the
learning pipeline, so as to avoid a scrambled database upon reboot.

* `rc-local-shutdown.service` is a systemd shim that will run locally
  installed shutdown scripts.

* `rc.local.shutdown` is installed in `/etc` and will run when the
  system shuts down. It needs to be edited to provide the user-name
  that is running the LXC containers.
