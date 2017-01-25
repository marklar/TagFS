

# Installation

To run TagFS, you'll need:
+ some supporting libraries and executables
+ TagFS itself


## Supporting Native Code

+ [Haskell Stack](https://docs.haskellstack.org/en/stable/README/)
+ PRCE (for Mac: `brew install pcre`)
+ FUSE (for Mac: [OsxFuse](https://osxfuse.github.io/))
+ Sqlite3 (installed by default on Mac)


## TagFS code

```
$ git clone git@github.com:marklar/TagFS.git
$ cd TagFS
$ stack setup
$ stack build
```

