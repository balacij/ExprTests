module Lib where

import Prelude hiding (concat)

import Data.Maybe (mapMaybe)
import qualified Data.Map as M

import Theory.DataDefinition (ddTest)
import Lang.ChunkDB (cdbTest)

{------------------------------------------------------------------------------
| Standard Stack template code below
------------------------------------------------------------------------------}

someFunc :: IO ()
someFunc = do
    putStrLn "someFunc"
    ddTest
    cdbTest
