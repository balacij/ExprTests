module Lang.ChunkDB where

import Lang.GenericClasses
import Lang.QDefinition

import Data.List (intercalate)
import qualified Data.Map as M
import Data.Typeable (Typeable, cast)

data Chunk where
    CHUNK :: (HasUID t, DrasilDumpable t, Typeable t) => t -> Chunk

instance HasUID         Chunk where uid  (CHUNK t) = uid t
instance DrasilDumpable Chunk where dump (CHUNK t) = dump t

type ChunkDB = M.Map UID Chunk

registerChunk :: (HasUID t, Typeable t, DrasilDumpable t) => t -> ChunkDB -> ChunkDB
registerChunk c = M.insert (uid c) (CHUNK c)

retrieveChunk :: Typeable a => ChunkDB -> UID -> Maybe a
retrieveChunk cdb u = do
    (CHUNK r) <- M.lookup u cdb
    cast r

cdb1 :: ChunkDB
cdb1 = M.empty

cdb2 :: ChunkDB
cdb2 = registerChunk qd3' $ registerChunk qd4' cdb1

mybePrintQD :: Maybe SimpleQDef' -> IO ()
mybePrintQD Nothing = putStrLn "No QD"
mybePrintQD (Just qd) = putStrLn $ simpleQDef'ToStr qd

cdbTest :: IO ()
cdbTest = do
    let chunks = M.elems cdb2
        joinedDump = intercalate "\n" (map dump chunks)

    putStrLn "---- { DUMP START } ----"
    putStrLn joinedDump
    putStrLn "---- { DUMP END } ----"

    putStrLn "Retrieving qd4', and printing:"
    let retrievedQD4' = retrieveChunk cdb2 "qd4'"
    mybePrintQD retrievedQD4'

    putStrLn "Intentionally failing a chunk retrieval, and printing:"
    let failedRetrieval = retrieveChunk cdb2 "fake qd uid"
    mybePrintQD failedRetrieval
