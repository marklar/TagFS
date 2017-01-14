Allows non-privileged users create their own file systems (FSs)
without editing kernel code.

Useful for writing virtual FSs.

Traditional FSs: save data to, and retrieve data from, mass storage

Virtual FSs: don't store data themselves.
Act as a view / translation of an existing FS.


A FS can be written as a wrapper over an underlying FS to:
+ manage its data and
+ provide an enhanced, feature-rich FS
  - e.g. cvsfs-fuse: FS interface for CVS
  - e.g. Wayback FS: backup mechanism to keep old copies of data

to do:
1. install a FUSE kernel module (OS X: https://osxfuse.github.io/)
2. use the FUSE library and API

