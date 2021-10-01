module Theory.DataDefinition where

import Lang.DefiningExpr
import Lang.Expr
import Lang.Exprs
import Lang.GenericClasses
import Lang.ModelExpr
import Lang.QDefinition

import Data.Maybe (mapMaybe)
import qualified Data.Map as M

{-~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
~ Ok, now DataDefinitions...
~
~ We need to be able to discern between code-usable DDs and non-code-usable.
~ We do this by forcing DDEs to contain Exprs (code-usable), while DDMEs
~ contain ModelExprs. Realistically, there's no reason why we need the non-code-usable ones
~ to always be "ModelExprs", but it doesn't really matter much (especially since we have the TTF constructors :^) ).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-}
data DataDefinition where
    DDE  :: String -> QDefinition (Expr a)      -> DataDefinition
    DDME :: String -> QDefinition (ModelExpr a) -> DataDefinition
    -- Alternatively, we can use this below to indicate a DD that shouldn't be used in code generation. TODO: This might be better! 
    -- DDME :: Express e => String -> QDefinition (e a) -> DataDefinition

instance HasUID DataDefinition where
    uid (DDE n _)  = n
    uid (DDME n _) = n

{- We need to build a function that grabs QDs. Similar to the existing ones:
```
-- | Extracts the 'QDefinition e' from a 'DataDefinition'.
qdFromDD :: DataDefinition -> Either SimpleQDef ModelQDef
qdFromDD (DDE  qd _) = Left qd
qdFromDD (DDME qd _) = Right qd

qdEFromDD :: DataDefinition -> Maybe SimpleQDef
qdEFromDD (DDE qd _) = Just qd
qdEFromDD _          = Nothing
```

Unfortunately, these don't work so well here..
```
qdFromDD :: DataDefinition -> Either (QDefinition (Expr a)) (QDefinition (ModelExpr b))
qdFromDD (DDE  _ qd) = Left qd
qdFromDD (DDME _ qd) = Right qd
```
This attempt causes a type issue. It has no way of matching the type signatures "a" with the "a" from the internally held "qd" from the DataDefinition.
We could resolve this by creating a type variable for DataDefinitions, but I'm not sure if we would want that. We would then need to have an untyped data wrapper
similar to QDefinitions above for 


Here's another attempt that's foiled because it can't recognize the output type formed by the internal qds of the dds!
```
actOnADDsQD :: DataDefinition -> (QDefinition (Expr a) -> r) -> (QDefinition (ModelExpr b) -> r) -> r
actOnADDsQD (DDE  _ qd) f _ = f qd
actOnADDsQD (DDME _ qd) _ g = g qd
```
-}

-- Finally! With a little bit of reading from https://en.wikibooks.org/wiki/Haskell/Existentially_quantified_types
-- we are able to at least
actOnADDsQD :: DataDefinition -> (forall a. QDefinition (Expr a) -> r) -> (forall b. QDefinition (ModelExpr b) -> r) -> r
actOnADDsQD (DDE _ qd) f _ = f qd
actOnADDsQD (DDME _ qd) _ f = f qd

dds :: [DataDefinition]
dds = [ dd2e, dd2me ]

-- TODO: Discuss "UnTypedQDefinitionExprs"
data UnTypedQDefinitionExprs = forall a. UTQDE (QDefinition (Expr a))

instance HasUID UnTypedQDefinitionExprs where
    uid (UTQDE qd) = uid qd

qdes :: [UnTypedQDefinitionExprs] -- A heterogeneous list!!
qdes = mapMaybe (\x -> actOnADDsQD x (Just . UTQDE) (const Nothing)) dds

-- >>> map uid qdes
-- ["qd2"]

-- Basic printer for DataDefinitions of all kinds
ddToStr0 :: Express e a => String -> String -> QDefinition (e a) -> String
ddToStr0 n k qd = "DataDefinition\nName: " ++ n ++ "\nKind: " ++ k ++ "\nQD: " ++ qd' ++ "\n"
    where qd' = meToStr $ express $ defnExpr3 qd

ddToStr :: DataDefinition -> String
ddToStr (DDE  name qd) = ddToStr0 name "Expr" qd
ddToStr (DDME name qd) = ddToStr0 name "ModelExpr" qd

dd2e :: DataDefinition
dd2e = DDE "dd2e" qd2

dd2me :: DataDefinition
dd2me = DDME "dd2me" qd2

type DataDefnMap = M.Map String DataDefinition

dataDefnMap :: M.Map String DataDefinition
dataDefnMap =
      M.insert (uid dd2me) dd2me 
    $ M.singleton (uid dd2e) dd2e

ddTest :: IO ()
ddTest = do
    putStrLn $ ddToStr dd2e
    putStrLn $ maybe "bad match" ddToStr $ M.lookup "dd2e" dataDefnMap
    putStrLn $ maybe "bad match" ddToStr $ M.lookup "dd2me" dataDefnMap

