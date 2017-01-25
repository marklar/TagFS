
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
+ `ln <source.txt> <target-dir>`
  - For tagging files. (Doesn't actually create a link.)
  - Does not allow 'linking' to a file of a different name. It merely
    adds any novel tags to the file.
+ `mv <source.txt> <target.txt>`
  - In TagFS, `mv` is also for tagging *and* untagging files.
  - If `target.txt` doesn't exist (anywhere), then:
    + If changing 'directories', `mv` associates a different set of
      tags with the file. It removes the last tag from `source.txt`'s
      path and adds any novel tags from `target.txt`'s path.
	+ If changing names, that works, too. In addition to changing the
      set of tags on the file, it also renames the file.
  - If a file named `target.txt` does already exist, `mv` refuses to
    overwrite it, reporting that 'File exists'. That may be surprising
    behavior, as outside of TagFS `source.txt` would simply overwrite
    `target.txt` (i.e. `source.txt` gets renamed to `target.txt`, and
    what was `target.txt` goes away). This is feasible, but I haven't
    gotten around to it yet.
+ `cp <source.txt> <target.txt>`
  - For copying *content* from one file to another.
  - So long as `target.txt` doesn't already exist (regardless of
    tags), `cp` works as expected, creating a new file and tagging it
    as you'd expect.
  - If `target.txt` *does* exist, `cp` currently creates an *empty*
  file `target(0).txt` (but with the proper tags).
+ `rm` (or `unlink`)
  - In TagFS, `rm` is for un-tagging files.
  - If `rm` from within a tag 'directory', the 'last' tag specified in
    the path is removed from the tag, but the file itself isn't
    removed from the FS.
  - If `rm` from root dir (`/`), the file is actually deleted.


Tags (directories):
+ `ls`
  - With -a option, shows `.dummy` file.
  - Includes all other tags as 'subdirs', even if they're empty (but
    for `.dummy`). Feature? Bug?
+ `mkdir`
  - Creates a new tag.
  - File `.dummy` is tagged with it.
+ `rmdir`
  - For untagging. Removes 'last' tag from path for each file in that
    path.
  - If no more files are tagged with this tag, the tag itself is
    deleted.


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
