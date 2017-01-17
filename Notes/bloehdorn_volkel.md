
# TagFS — Tag Semantics for Hierarchical File Systems

http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.60.4187&rep=rep1&type=pdf

## vs. Traditional (Hiearchical) Systems

### Tradition System

file allocation table:
+ index structure providing a mapping
+ k: access path.  v: corresponding blocks of info

structure of access paths (e.g. /foo/bar/baz.txt)
-> perceived organization of files into dirs

path - decomposable into seq of dirs + file name

### Tagging System

tag:
+ UNARY predicate over resources (i.e. not k:v). easier transition
  from traditional FSs.
+ imply set - combine w/ union, intersection, complement


## Retrieval

the view of a directory: union of these 2 functions:
+ view       :: Location -> [File]
+ subfolders :: Location -> [Tag]
  - shows _other_ tags on files matching location (for refinement)
  - shown: <tag> (<num-results>).  e.g. "favorites (32)"

File ~ Resource
Tag ~ Folder

query result ~ ([File], [SubFolder])

data Query :: Query [Tag]   -- or Set, really


### Delete

Remove last tag (or should it be ALL tags?)

`delete :: Location -> File -> ()`

To delete file (for real), add tag 'delete'


### Copy

Adds tags from src 'dir' to target 'dir'.

`copy :: Location {-src-} -> Location {-target-} -> File -> ()`


### Move

Delete + Copy

Perhaps: 'File' is take from end of src Location?

```haskell
-- move :: Location {-src-} -> Location {-target-} -> ()
move :: Location {-src-} -> Location {-target-} -> File -> ()
```

### Mkdir

If tag doesn't exist, create.

Since there are no files yet, create special placeholder file
"just_created", tagged w/ all tags implied by the current location +
new tag. Rm file once a 'real' file gets that tag.

`mkdir :: Location -> ()`


### Other Dir Ops

Execute corresponding command for each file at source Location.

```
mvdir :: Location -> Location -> ()
cpdir :: Location -> Location -> ()
rmdir :: Location -> Location -> ()
```

### Importing Files

steps:
1. calc MD5 hash of external file's contents as resource key
2. insert external file into storage
3. assign all tags (from ExternalLocation) to resource key
4. orig filename + disambiguation number (e.g. "-1") + extension

put :: ExternalLocation -> Location -> ()


## Managing Tag Assignments

RDF: Resource Description Framework
metadata data model
subject-predicate-object structure
(like from W3C's semantic web)

(File, User, Tag)
This allows for collaborative tagging.

Each part has these fields:

+ File:
  - name
  - hash
  - size
  - createdAt
+ User:
  - login
  - password
+ Tag:
  - label

## Storage Backend

For simplicity and backup -> classic FS dir.

Don't perform well with highly populated folders, so 2-level index
strategy, e.g. storing a file 'abcde.txt' as 'a/ab/abcde.txt'.


## Virtual FS

Virtual FS - set of virtual folders which appear w/in existing
tree-structured FSs.

Virtual directory:
+ abstracts storage locations
+ lists files based on a query which dynamically determines the
  content

"virtual hierarchy"
