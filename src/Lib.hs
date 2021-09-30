module Lib where

import Prelude hiding (concat)

import Data.Maybe (mapMaybe)
import qualified Data.Map as M

import Theory.DataDefinition

{------------------------------------------------------------------------------
| Standard Stack template code below
------------------------------------------------------------------------------}

someFunc :: IO ()
someFunc = do
    putStrLn "someFunc"
    ddTest
