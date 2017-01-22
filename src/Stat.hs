{-# LANGUAGE UnicodeSyntax       #-}

module Stat
  ( dirStat
  , fileStat
  , getFileSystemStats
  ) where


import           System.Fuse
import           System.Posix.Files   -- for file modes
import           Debug                   (dbg)


-- ^ What should these be?
getFileSystemStats :: String -> IO (Either Errno FileSystemStats)
getFileSystemStats _ =
  return $ Right FileSystemStats { fsStatBlockSize = 512
                                 , fsStatBlockCount = 1
                                 , fsStatBlocksFree = 1
                                 , fsStatBlocksAvailable = 1
                                 , fsStatFileCount = 5
                                 , fsStatFilesFree = 10
                                 , fsStatMaxNameLength = 255
                                 }


dirStat ∷ FuseContext → FileStat
dirStat ctx = FileStat { statEntryType = Directory
                       , statFileMode = dirFileMode
                       , statLinkCount = 2    -- ??
                       -- Owners
                       -- record members for FuseContext
                       , statFileOwner = fuseCtxUserID ctx
                       , statFileGroup = fuseCtxGroupID ctx
                       -- ??
                       , statSpecialDeviceID = 0
                       , statFileSize = 4096
                       , statBlocks = 1
                       -- times: Access, Modification, and StatusChange
                       , statAccessTime = 0
                       , statModificationTime = 0
                       , statStatusChangeTime = 0
                       }


fileStat ∷ FuseContext → FileStat
fileStat ctx = FileStat { statEntryType = RegularFile
                        , statFileMode = fileFileMode
                        , statLinkCount = 1
                        , statFileOwner = fuseCtxUserID ctx
                        , statFileGroup = fuseCtxGroupID ctx
                        , statSpecialDeviceID = 0
                        , statFileSize = 0
                        , statBlocks = 1
                        -- times: Access, Modification, and StatusChange
                        , statAccessTime = 0
                        , statModificationTime = 0
                        , statStatusChangeTime = 0
                        }


{- | unionFileModes ∷ FileMode → FileMode → FileMode
Combines the 2 file modes into 1 that contains modes that appear in either.
`FileMode`s are in System.Posix.Files
-}
dirFileMode =
  foldr1 unionFileModes [ ownerReadMode, ownerExecuteMode, ownerWriteMode
                        , groupReadMode, groupExecuteMode
                        , otherReadMode, otherExecuteMode
                        ]

fileFileMode =
  foldr1 unionFileModes [ ownerReadMode, ownerWriteMode
                        , groupReadMode
                        , otherReadMode
                        ]
