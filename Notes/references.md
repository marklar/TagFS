
# TagFS

## Papers

TagFS: A simple tag-based filesystem
http://web.mit.edu/6.033/2011/wwwdocs/writing-samples/sbezek_dp1.pdf

TagFS — Tag Semantics for Hierarchical File Systems
http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.60.4187&rep=rep1&type=pdf


## Fuse

Haskell library:
https://hackage.haskell.org/package/HFuse

example Hs code:
https://github.com/cgag/haskell-fuse-test
(in-memory filesystem)

Haskell FUSE example:
https://github.com/timbod7/secretfs

libfuse reference impl (C):
https://github.com/libfuse/libfuse

tutorial:
http://www.ibm.com/developerworks/linux/library/l-fuse/


## Other Impls

Haskell:
https://github.com/ffwng/tagfs
Rather than a DB, it uses "tag files".
e.g. "myFile.txt" has an associated "myFile.txt.tags", w/ one tag per line

https://github.com/marook/tagfs

https://www.youtube.com/watch?v=JQWU0h12huc

http://tmsu.org/


## DBs

http://www.yesodweb.com/book/persistent
https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/persistent-db


### Persistent

library for type-safe data serialization (including to SQLite backend)
(http://www.yesodweb.com/book/persistent)
https://hackage.haskell.org/package/persistent-2.6/docs/Database-Persist.html


### Esqueleto

Unnecessary?

type-safe EDSL for SQL queries
https://hackage.haskell.org/package/esqueleto
