
# Persistent

Type-safe, universal data store interface. Useful static guarantees.


## Convenient data modeling

Uses code generation (Template Hs)

```haskell
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    name String
    age Int Maybe   -- Maybe Int?
    deriving Show
BlogPost
    title String
    authorId PersonId
    deriving Show
|]

main ∷ IO ()
main = runSqlite ":memory:" $ do
    -- set up data model
    runMigration migrateAll
	…
```


## Insert, Select, Delete

Simple examples

```haskell
    -- insert two Persons, getting back their DB ids
    johnId ← insert $ Person "John Doe" $ Just 35
    janeId ← insert $ Person "Jane Doe" Nothing

    -- insert two BlogPosts, associate w/ Persons
    insert $ BlogPost "My fr1st p0st" johnId
    insert $ BlogPost "One more for good measure" johnId

    -- [] - TH syntax
    -- Get the IDs of all BlogPosts w/ authorId of John's; keep only 1st.
    oneJohnPost ← selectList [BlogPostAuthorId ==. johnId] [LimitTo 1]

    -- print the post - the whole thing, not just the ID, right?
    -- What's [Entity BlogPost]?
    -- liftIO $ print (oneJohnPost ∷ [Entity BlogPost])
    liftIO $ print oneJohnPost

    -- 'get': fetches the entity
    john ← get johnId
    -- liftIO $ print (john ∷ Maybe Person)
    liftIO $ print john

    -- Delete Jane.
    delete janeId
	-- Delete John's BlogPosts.
    deleteWhere [BlogPostAuthorId ==. johnId]
```

## PersistValue (datatype)

Sum type, representing serializable data.

The SQLite backend knows how to translate vals into SQLite vals.


## PersistField (typeclass)

PersistField instance ~ DB column

Defines marshaling: Hs datatype <=> PersistValue


## PersistEntity (typeclass)

PersistEntity instance ~ DB table


