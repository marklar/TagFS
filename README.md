
# TagFS


## Deploying TagFS

First, install TagFS: [Install](docs/Install.md).

Then run it:

1. Modify env vars in shell script `./run` as desired.
2. `$ chmod +x run`
3. Launch the TagFS background program: `$ ./run`

And finally, go to the TagFS volume: `$ cd <your-mount-point>` (=
`$TAGFS_MOUNT_POINT`).

When you're done using TagFS, you may unmount its filesystem and kill
off its background program:

1. `cd` outside our mount point
2. `$ umount <your-mount-point>`. (That's on Mac. On Linux, I believe
   it's `$ fusermount -u <your-mount-point>`.)



## FS-related Commands

### Interesting behavior

In a tag-based FS, filenames must be unique. But in a traditional FS,
`cp` and `mv` can be used to specify a new name (as well as new
directory) for a file. TagFS must be careful to thwart this behavior
as possible.


Files:
+ `cp <source.txt> <target.txt>`
  - So long as `target.txt` doesn't already exist (regardless of
  tags), `cp` works as expected.
  - If `target.txt` *does* exist, `cp` currently creates an *empty*
  file `target(0).txt` (but with the proper tags).
+ `mv <source.txt> <target.txt>`
  - In TagFS, `mv` is for tagging files.
  - If `target.txt` doesn't exist (anywhere), then:
    + If changing 'directories', `mv` associates a different set of
    tags with the file.
	+ If changing names, that works, too.
  - If a file named `target.txt` does already exist, `mv` refuses to
    overwrite it, reporting that 'File exists'. That may be surprising
    behavior, as outside of TagFS `source.txt` would simply overwrite
    `target.txt` (i.e. `source.txt` gets renamed to `target.txt`, and
    what was `target.txt` goes away).
+ `rm`
  - Works as designed.
  - It removes tags from file. If `rm` from root dir (`/`), the file
    is actually removed. If `rm` from within a tag 'directory', the
    'last' tag specified in the path is removed from the tag, but the
    file itself isn't removed from the FS.


Tags (directories):
+ `ls`
  - shows hidden file (`.dummy`)
  - shows other tags, even tho' they're empty (but for `.dummy`)
+ `mkdir`
  - creates a new tag
  - contains `.dummy`
+ `rmdir`
  - Fails to remove dir from display, even though empty (removes `.dummy`)


### Work as you'd expect

Viewing file contents
+ `cat`
+ `head`
+ `less`
+ `more`
+ `tail`

Navigating
+ `cd`
+ `file`
+ `find`
+ `pwd`
+ `wc`

Modifying
+ `> <fn>` (i.e. `truncate`)


### Not yet working

+ `touch` - Creates file if didn't exist, but doesn't update times.
+ `chmod` - no-op
+ `chown` - no-op
+ `chgrp` - no-op
+ `ln` - not implemented

Disk-related
+ `df` - shows bogus disk usage info
+ `du`


## Implementation Gotchas


### DB Threading - use `clone`

Sqlite3 does not support more than one active statement for a single
connection, so you must use a different connection to the database for
each simultaneous query you wish to use.

To do so, use:
`Database.HDBC.clone :: conn -> IO conn`

`clone` establishes a separate physical connection to the same DB. So
first establish one connection with the driver-specific connection
function, and then clone it for each additional connection.



### Working Directory

FUSE's working directory is not the one you're in when you launch
TagFS, but rather `/`.

This means: *don't use relative paths*.

The moment your FUSE code attempts to write to a file (a DB or
logfile, for example) specified via a relative path, you're in for a
world of hurt.
