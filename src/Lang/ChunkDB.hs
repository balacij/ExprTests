{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications #-}
module Lang.ChunkDB where

import Lang.GenericClasses
import Lang.QDefinition

import Data.List (intercalate, nub)
import qualified Data.Map as M
import Data.Typeable (Typeable, cast, TypeRep, typeRep, typeOf)
import Data.Maybe (mapMaybe)
import Data.Proxy
import Data.Bifunctor

type JunkInformation = String
data Junk = Junk UID JunkInformation
    deriving Typeable

instance HasUID         Junk where uid (Junk u _)     = u
instance DrasilDumpable Junk where dump (Junk u info) = "Junk { uid = '" ++ u ++ "' ; info = '" ++ info ++ "'"

data Chunk where
    CHUNK :: (HasUID t, DrasilDumpable t, Typeable t) => t -> Chunk

instance HasUID         Chunk where uid  (CHUNK t) = uid t
instance DrasilDumpable Chunk where dump (CHUNK t) = dump t

type ChunkDB = M.Map UID Chunk

chunksToChunkDB :: (HasUID t, Typeable t, DrasilDumpable t) => [t] -> ChunkDB
chunksToChunkDB = M.fromList . map (\x -> (uid x, CHUNK x))

registerChunk :: (HasUID t, Typeable t, DrasilDumpable t) => t -> ChunkDB -> ChunkDB
registerChunk c = M.insert (uid c) (CHUNK c)

retrieveChunk :: Typeable a => ChunkDB -> UID -> Maybe a
retrieveChunk cdb u = do
    (CHUNK r) <- M.lookup u cdb
    cast r

retrieveChunksByType :: (HasUID t, DrasilDumpable t, Typeable t) => ChunkDB -> TypeRep -> [t]
retrieveChunksByType cdb tr = relevantChunks
    where
        allChunks = M.toList cdb
        relevantChunks = mapMaybe (\(_, CHUNK c) -> if typeOf c == tr then cast c else Nothing) allChunks
        -- ALTERNATIVE: 
        -- relevantChunks = mapMaybe (\(_, CHUNK c) -> cast c) allChunks
        --
        -- This alternative appears to often work better, but I might be misunderstanding why. 
        --
        -- Here, if I replace `if typeOf c == tr then cast c else Nothing` with just `cast c`, the below discussion of 0 becomes nil, but then it allows too much...
        -- I thought this equivalence checking would be useless, but it turns out it wasnt!

retrieveChunksByTypeInChunkBox :: ChunkDB -> TypeRep -> [Chunk]
retrieveChunksByTypeInChunkBox cdb tr = relevantChunks
    where
        allChunks = M.toList cdb
        relevantChunks = mapMaybe (\(_, it@(CHUNK c)) -> if typeOf c == tr then Just it else Nothing) allChunks

type ChunksByTypeRep = M.Map TypeRep ChunkDB -- aside: The ``value'' here could also be changed to contain precalculated views of the inner ChunkDB

dumpChunkDBToTypeRepMap :: ChunkDB -> ChunksByTypeRep
dumpChunkDBToTypeRepMap cdb = allRegistered
    where
        -- gather a list of registered types
        knownChunkTypes :: [TypeRep]
        knownChunkTypes = nub . map (typeOf . snd) $ M.toList cdb

        -- gather types with a list of all registered chunks that are of that type
        chunksWithTypes :: [(TypeRep, [Chunk])]
        chunksWithTypes = map (\x -> (,) x $ retrieveChunksByTypeInChunkBox cdb x) knownChunkTypes

        -- reconcile everything found, into a single ChunksByTypeRep map
        allRegistered :: ChunksByTypeRep
        allRegistered = M.fromList $ map (second chunksToChunkDB) chunksWithTypes

type ChunkDB' = (ChunkDB, ChunksByTypeRep)

registerChunk' :: (HasUID t, Typeable t, DrasilDumpable t) => t -> ChunkDB' -> ChunkDB'
registerChunk' c (cdb, trcdb) = (cdb', trcdb')
    where
        u      = uid c
        chk    = CHUNK c
        cdb'   = M.insert u chk cdb
        trcdb' = M.alter
                    (maybe
                        (Just $ M.singleton u chk)
                        (Just . M.insert u chk))
                    (typeOf c)
                    trcdb

retrieveChunk' :: Typeable a => ChunkDB' -> UID -> Maybe a
retrieveChunk' (cdb, _) u = do
    (CHUNK r) <- M.lookup u cdb
    cast r

retrieveChunksByType' :: Typeable a => ChunkDB' -> TypeRep -> [a]
retrieveChunksByType' (_, trcdb) tr = maybe [] (mapMaybe ((\(CHUNK c) -> cast c) . snd) . M.toList) (M.lookup tr trcdb)

cdb1 :: ChunkDB
cdb1 = M.empty

cdb2 :: ChunkDB
cdb2 = registerChunk qd3'
    $ registerChunk (Junk "junk1" "useless information")
    $ registerChunk qd4'
    $ registerChunk (Junk "junk2" "more useless information")
    cdb1

maybeQDToStr :: Maybe SimpleQDef' -> String
maybeQDToStr = maybe "No QD" simpleQDef'ToStr

infoDump :: (DrasilDumpable t) => [t] -> String
infoDump = intercalate "\n" . map dump

cdbTest :: IO ()
cdbTest = do
    putStrLn "---- { FULL DUMP START } ----"
    putStrLn $ infoDump $ M.elems cdb2
    putStrLn "---- { FULL DUMP  END  } ----\n"

    putStrLn "Retrieving qd4', and printing:"
    let retrievedQD4' = retrieveChunk cdb2 "qd4'"
    putStrLn $ maybeQDToStr retrievedQD4'
    putStrLn ""

    putStrLn "Intentionally failing a chunk retrieval, and printing:"
    let failedRetrieval = retrieveChunk cdb2 "fake qd uid"
    putStrLn $ maybeQDToStr failedRetrieval
    putStrLn ""

    let junkInst = Junk "" ""
    let foundJunk = retrieveChunksByType cdb2 (typeOf junkInst) :: [Junk] -- This explicit type signature is required! How can I get rid of needing pureJunk however?
    let foundJunk' = retrieveChunksByType cdb2 (typeRep Junk) :: [Junk] -- Using this gives 0 when we have the above `typeOf c == tr` check
    let foundJunk'' = retrieveChunksByType cdb2 (typeRep (Proxy @Junk)) :: [Junk]
    putStrLn $ "Junk found: " ++ show (length foundJunk)
    putStrLn $ "Junk' found: " ++ show (length foundJunk')
    putStrLn $ "Junk'' found: " ++ show (length foundJunk')
    putStrLn "---- { FOUND JUNK DUMP START } ----"
    putStrLn $ infoDump foundJunk
    putStrLn "---- { FOUND JUNK DUMP  END  } ----\n"

    let foundQDs = retrieveChunksByType cdb2 (typeOf qd4') :: [SimpleQDef']
    putStrLn $ "QDs found: " ++ show (length foundQDs)
    putStrLn "---- { FOUND QDs DUMP START } ----"
    putStrLn $ infoDump foundQDs
    putStrLn "---- { FOUND QDs DUMP  END  } ----"

    print (typeOf junkInst)       -- "Junk"
    print (typeRep Junk)          -- "[Char] -> Junk" (seemingly missing a parameter?)
    print (typeRep (Proxy @Junk)) -- "Junk"

    print (typeOf junkInst == typeRep Junk)          -- False
    print (typeOf junkInst == typeRep (Proxy @Junk)) -- True
    print (typeRep Junk    == typeRep (Proxy @Junk)) -- False


{-

TODO:
- need a templatehaskell function which can generate functions for chunks automatically:
    - ability to grab a well-typed "thing" from a ChunkDB with a specific type
    - ability to grab a list of well-typed "things" fromm a ChunkDB with a specific type

-}
