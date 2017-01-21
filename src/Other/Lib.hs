{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}


module Lib
    ( runFuse
    ) where


import           Control.Concurrent.MVar
import           Control.Exception.Base  (SomeException)
import           Data.ByteString         (ByteString)
import qualified Data.ByteString.Char8   as B
import qualified Data.HashMap.Strict     as M
import           Data.Monoid             ((<>))
import           Foreign.C.Error
import           System.Fuse
import           System.Posix.Files
import           System.Posix.Types
import Data.List (intercalate)
import Data.List.Split (splitOn)



data DUMMY = DUMMY
debugFile = "/home/cgag/src/fuse/haskell/debug"
dbg msg = B.appendFile debugFile (msg <> "\n")


-- contents:
--   Dir:  FilePath → Entry
--   File: blob
data Contents = Dir  { d_contents ∷ M.HashMap FilePath Entry }
              | File { f_contents ∷ !ByteString }
              deriving Show

-- Both Dirs and Files have FileStats (from FUSE).
-- They have different Contents.
data Entry = Entry
    { stat ∷ FileStat
    , contents ∷ Contents
    } deriving Show


{- | Does nothing, really. This action uses an Either, so returns a
   Right _. It must return a filehandle-like value which FUSE ignores
   but simply passes back to us to use in other operations.
-}
helloOpen :: FilePath
          -> OpenMode         -- FUSE: ReadOnly | WriteOnly | ReadWrite
          -> OpenFileFlags    -- FUSE: append | exclusive | noctty | nonBlock | trunc
          -> IO (Either Errno DUMMY)
helloOpen fpath mode flags = return (Right DUMMY)


{- | In case of success, must return ByteCount bytes of the ByteString
   contents of a file.
-}
helloRead :: FileStore
          -> FilePath
          -> DUMMY
          -> ByteCount
          -> FileOffset
          -> IO (Either Errno ByteString)
helloRead fileStore fpath handle bc offset = do
    fileMap <- readMVar fileStore
    let (_:fname) = fpath
    dbg ("Reading " <> B.pack fname)
    case M.lookup fname fileMap of
        Just (Entry _ (File contents)) -> do
            dbg ("Read " <> B.pack fname <> ", got: " <> contents)
            return (Right contents)
        Just (Entry _ (Dir contents)) -> do
            dbg ("Read dir " <> B.pack fname)
            return (Right "DIRCONTENTS")
        Nothing -> do
            dbg ("Failed to read: " <> B.pack fname)
            return (Left eNOENT)

type FileStore = MVar (M.HashMap FilePath Entry)

helloReadDirectory :: FileStore
                   -> FilePath -> IO (Either Errno [(FilePath, FileStat)])
helloReadDirectory fileStore fpath =
    case fpath of
        "/" -> do
            fileMap <- readMVar fileStore
            return (Right (fileList fileMap))
        _ -> return (Left eNOENT)
  where
    fileList = M.foldlWithKey' (\acc k v -> (k, stat v):acc) []

helloCreateDevice :: FileStore   -- MVar w/ map: FilePath → Entry
                  -> FilePath    -- FilePath ~ String
                  -> EntryType   -- FUSE: The Unix type of a node in the filesystem (e.g. RegularFile, Directory, etc.).
                  -> FileMode    -- System.Posix.Types
                  -> DeviceID    -- System.Posix.Types
                  -> IO Errno    -- Foreign.C.Error
helloCreateDevice fileStore fpath entryType mode did = do
    dbg ("creating device with path: " <> B.pack fpath)
    ctx <- getFuseContext
    fileMap <- readMVar fileStore
    case entryType of
        RegularFile -> do
            let (fname:restOfPath) = filter (/= "") . splitOn "/" $ fpath
            -- functional update: i.e., the std FileStat, except setting statFileMode
            let newStat = (fileStat ctx) { statFileMode = mode }
            _ <- swapMVar fileStore (M.insert fname (Entry newStat (File "")) fileMap)
            return eOK
        _ -> do
          dbg ("Failed to create unknown device type with path: " <> B.pack fpath)
          return eNOENT

helloCreateDirectory :: FileStore -> FilePath -> FileMode -> IO Errno
helloCreateDirectory fileStore fpath mode = do
    dbg ("creating directory with path: " <> B.pack fpath)
    ctx <- getFuseContext
    fileMap <- readMVar fileStore
    let (_:fname) = fpath
    let newStat = (dirStat ctx) { statFileMode=mode }
    swapMVar fileStore
             (M.insert fname
                 (Entry newStat
                     (Dir (M.fromList [(".",  Entry { stat=dirStat ctx
                                                    , contents=(Dir M.empty)})
                                      ,("..", Entry { stat=dirStat ctx
                                                    , contents=(Dir M.empty)})
                                      ])))
                 fileMap)
    return eOK


helloOpenDirectory :: FilePath -> IO Errno
helloOpenDirectory "/" = return eOK
helloOpenDirectory _   = return eNOENT

helloGetFileSystemStats :: String -> IO (Either Errno FileSystemStats)
helloGetFileSystemStats str =
    return $ Right FileSystemStats
        { fsStatBlockSize = 512
        , fsStatBlockCount = 1
        , fsStatBlocksFree = 1
        , fsStatBlocksAvailable = 1
        , fsStatFileCount = 5
        , fsStatFilesFree = 10
        , fsStatMaxNameLength = 255
        }

helloGetFileStat :: FileStore -> FilePath -> IO (Either Errno FileStat)
helloGetFileStat fileStore fpath = do
    dbg ("getting file stat: " <> B.pack fpath)
    ctx <- getFuseContext
    fileMap <- readMVar fileStore
    case fpath of
        "/" -> return $ Right (dirStat ctx)
        _ -> do
            let (fname:restOfPath) = filter (/= "") . splitOn "/" $ fpath
            dbg ("looking up fname of: " <> B.pack fname)
            case M.lookup fname fileMap of
                Nothing -> do
                    dbg ("Failed to find " <> B.pack fname)
                    dbg ("Used map" <> B.pack (show fileMap))
                    return (Left eNOENT)
                Just (Entry stat (File _)) -> do
                    dbg ("Found file")
                    return (Right stat)
                Just (Entry stat (Dir _)) -> do
                    dbg "Found dir"
                    error "need a function that recursively looks up stats"

helloWrite :: FileStore
           -> FilePath
           -> DUMMY
           -> ByteString
           -> FileOffset
           -> IO (Either Errno ByteCount)
helloWrite fileStore fpath _ bytes offset = do
    dbg ("writing file: " <> B.pack fpath)
    fileMap <- readMVar fileStore
    let (_:fname) = fpath
    case M.lookup fname fileMap of
        Nothing -> do
          dbg $ "Write: didn't find file (" <> B.pack fname <> ")"
          return (Left eNOENT)
        Just (Entry stat contents) -> do
            case contents of
                File fcontents -> do
                    dbg ("Writing to file: -- " <> B.pack fname)
                    writeFile fileMap stat fname fcontents
                Dir dcontents -> do
                    dbg "Writing to DIR, what"
                    return (Left eNOENT)
  where
    writeFile fileMap stat fname contents = do
        let newContents = B.take (ioffset - 1) contents <> bytes
        swapMVar fileStore
                 (M.insert fname
                           (Entry
                              (stat {
                                  statFileSize = fromIntegral . B.length $ newContents
                              })
                              (File newContents))
                          fileMap)
        return $ Right (fromIntegral . B.length $ bytes)
    ioffset = fromIntegral offset
    bytesWritten = fromIntegral (B.length bytes)

helloSetFileTimes :: FileStore -> FilePath -> EpochTime -> EpochTime -> IO Errno
helloSetFileTimes fileStore fpath t1 t2 = return eOK

helloAccess :: FilePath -> Int -> IO Errno
helloAccess _ _ = return eOK

helloRemoveLink :: FileStore -> FilePath -> IO Errno
helloRemoveLink fileStore fpath = do
    fileMap <- readMVar fileStore
    let (_:fname) = fpath
    case M.lookup fname fileMap of
        Nothing -> return eNOENT
        Just _ -> do
            swapMVar fileStore (M.delete fname fileMap)
            return eOK



-- TODO: understand and anki all these options
dirStat ctx = FileStat { statEntryType = Directory
                       -- unionFileModes ∷ FileMode → FileMode → FileMode
                       -- Combines the 2 file modes into 1 that contains modes that appear in either.
                       -- `FileMode`s are in System.Posix.Files
                       , statFileMode = foldr1 unionFileModes
                                          [ ownerReadMode
                                          , ownerExecuteMode
                                          , ownerWriteMode
                                          , groupReadMode
                                          , groupExecuteMode
                                          , otherReadMode
                                          , otherExecuteMode
                                          ]
                       , statLinkCount = 2
                       -- data FuseContext = FuseContext
                       --     { fuseCtxUserID :: UserID
                       --     , fuseCtxGroupID :: GroupID
                       --     , fuseCtxProcessID :: ProcessID
                       --     }
                       , statFileOwner = fuseCtxUserID ctx
                       , statFileGroup = fuseCtxGroupID ctx
                       , statSpecialDeviceID = 0
                       , statFileSize = 4096
                       , statBlocks = 1
                       , statAccessTime = 0
                       , statModificationTime = 0
                       , statStatusChangeTime = 0
                       }

fileStat ctx = FileStat { statEntryType = RegularFile
                        -- unionFileModes ∷ FileMode → FileMode → FileMode
                        -- Combines the 2 file modes into 1 that contains modes that appear in either.
                        -- `FileMode`s are in System.Posix.Files
                        , statFileMode = foldr1 unionFileModes
                                           [ ownerReadMode
                                           , ownerWriteMode
                                           , groupReadMode
                                           , otherReadMode
                                           ]
                        , statLinkCount = 1
                        , statFileOwner = fuseCtxUserID ctx
                        , statFileGroup = fuseCtxGroupID ctx
                        , statSpecialDeviceID = 0
                        , statFileSize = 0
                        , statBlocks = 1
                        , statAccessTime = 0
                        , statModificationTime = 0
                        , statStatusChangeTime = 0
                        }


runFuse :: IO ()
runFuse = do
  -- Context of the program doing the current FUSE call.
  -- Provides the userID and groupID.
    ctx <- getFuseContext

  -- Create an MVar containing a strict hashmap.
  -- From file/dir name to Entry.
  -- (An Entry consists of a `stat` and `contents`.)
    fileList <- newMVar (M.fromList
                          [(".",  Entry { stat = dirStat ctx
                                        , contents = Dir M.empty
                                        })
                          ,("..", Entry { stat = dirStat ctx
                                        , contents = Dir M.empty
                                        })
                          ])

    fuseMain (buildOps fileList) defaultExceptionHandler
  where
    buildOps :: FileStore -> FuseOperations DUMMY
    buildOps fileStore =
        defaultFuseOps
            {
              fuseOpen               = helloOpen                       -- does nothing; returns Right DUMMY
            , fuseAccess             = helloAccess                     -- does nothing; eOK
            , fuseGetFileSystemStats = helloGetFileSystemStats         -- does nothing: dummy FS stat record
            , fuseOpenDirectory      = helloOpenDirectory              -- return eNOENT (Foreign.C.Error)

            , fuseRead               = helloRead fileStore             -- if file, contents. if dir, a static str.
            , fuseReadDirectory      = helloReadDirectory fileStore    -- if '/', returns map: path → stat
            , fuseGetFileStat        = helloGetFileStat fileStore      -- return FileStat (if matching)
            , fuseCreateDevice       = helloCreateDevice fileStore     -- creates a file
            , fuseWrite              = helloWrite fileStore
            , fuseRelease            = \_ _ -> return ()
            , fuseRemoveLink         = helloRemoveLink fileStore
            , fuseCreateDirectory    = helloCreateDirectory fileStore

            , fuseSetFileTimes       = helloSetFileTimes fileStore
            }
