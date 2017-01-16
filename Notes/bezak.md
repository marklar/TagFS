
## Tags

stores tag associations alongside file data in non-volatile memory
64-bit file and device numbering scheme (?)

### Tag and File Storage

storage device contains four sections:
+ Tag List: tags and their associated list of File IDs (FIDs)
+ Tag Tree: allows for quick tag lookup by name and links to elements of the Tag List
+ File Node section: maps FIDs to data blocks
+ Data Block section: contains the contents of files

#### Tag List

Tag List:
+ block of memory split into numbered Tag Nodes
+ contains a bitmap of Tag ID availability (so that new tags can reuse deprecated Tag IDs)

Tag Node:
+ fixed size
+ represents a tag on the device
+ holds tag metadata
+ provides a compact numbering scheme (Tag IDs) for referencing a tag.
+ contains:
  - the tag name (fixed length string)
  - a count of files containing that tag,
  - a fixed-length array of FIDs, and
  - a pointer to a data block that may contain a longer array of FIDs if necessary.

Data Block:
+ may also link to another data block (forming a linked-list) as needed
+ tag node's location on disk - determined from Tag ID

Tag Tree:
+ B+ Tree - allows for quick search
+ keys: tags
+ leaf nodes: point to Tag Node (in Tag List)


## Scopes
