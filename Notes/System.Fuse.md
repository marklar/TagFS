
# System.Fuse

The binding tries to follow as much as possible current Haskell POSIX interface in
+ System.Posix.Files
+ System.Posix.Directory

## Using

record `FuseOperations`
+ one field per FS operation (callable by FUSE)
+ each actions must return a POSIX error code (aka Errno)
  - use `Either`, OR
  - return `eOK` (in case of success)

(Read and writes: `ByteString`)

function `fuseMain`
+ binds provided operations to FS ones
+ `fh` - type of filehandle returned by `fuseOpen` (and passed to all ops)


## fuseMain

Minimum that `main` must call.

provide it:
- FuseOperations record (w/ FS impl)
- exception handler - convert Hs exceptions to Errno

performs:
+ options
  - parses cmd-line opts (`-d`, `-s` and `-h`)
  - passes all opts after `--` to `fusermount` (below)
+ mounts the FS (by calling `fusermount`)
+ signal handlers
  - installs POSIX signal handlers for:
    + keyboardSignal
    + lostConnection
    + softwareTermination
    + openEndedPipe
  - registers an exit handler to unmount the FS (on program exit)
+ event loop	
  - 'installs' the operations (below)
  - calls FUSE event loop


### fusermount

fusermount: mount and unmount FUSE filesystems
`fusermount [OPTIONS] MOUNTPOINT`
options:
  -h  print help
  -V  print version
  -o OPTION[,OPTION...] (mount options)
  -u  unmount
  -q  quiet
  -z  lazy unmount

### cmd-line opts?



### Operations

+ entire FS
  - init - Initializes the FS. Called before all other operations.
  - destroy - Called on FS exit to allow cleanup.
+ general inodes
  - getFileStat (lstat) - info about inode (number, owner, last access)
  - createDevice (mknod) - creates file (either regular or special)
  - rename (mv)
  - setFileMode (chmod)
  - setOwnerAndGroup (chown)
  - setFileSize (truncate)
  - setFileTimes (utime)
+ links
  - createLink (link)
  - createSymbolicLink (symlink)
  - readSymbolicLink (readlink) - gets FilePath
  - removeLink (unlink)
+ directory
  - createDirectory (mkdir)
  - removeDirectory (rmdir)
  - openDirectory (opendir)
  - readDirectory (readdir) - Entire contents of dir: return as list of tuples
  - releaseDirectory (closedir)
+ on filehandle (fh)
  - open (open) - get
    + success: Right of filehandle-like value
    + fh will be passed to future file ops
  - read (pread) - read from file from offset
  - write (pwrite) - write to file at offset
  - flush - called when `close` is called
  - release - called when file descriptors are closed
+ getFileSystemStats (statfs)
+ synchronizeFile (fsync)
+ synchronizeDirectory
+ access - Called for `access` system call. Check file access
  permissions (unless the default_permissions mount option is
  given)

