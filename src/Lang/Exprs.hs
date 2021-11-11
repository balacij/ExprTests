module Lang.Exprs where

import Lang.Expr (Expr)
import qualified Lang.Expr as E

import Lang.ModelExpr (ModelExpr)
import qualified Lang.ModelExpr as M

-- | The standard expression language in TTF form
class ExprC r where
    int :: Integer -> r Integer
    str :: String -> r String
    add :: Num a => r a -> r a -> r a
    concat :: r String -> r String -> r String
    eq  :: Eq a => r a -> r a -> r Bool

instance ExprC Expr where
    int i = E.Int i
    str s = E.Str s
    add l r = E.Add l r
    eq l r = E.Eq l r
    concat l r = E.Concat l r

instance ExprC ModelExpr where
    int i = M.Int i
    str s = M.Str s
    add l r = M.Add l r
    eq l r = M.Eq l r
    concat l r = M.Concat l r

-- | ModelExpr in TTF form.
-- 
--  ( only the differences between Expr and ModelExpr will appear here. )
class ModelExprC r where
    extraConstructor :: r String -> r String -> r String

instance ModelExprC ModelExpr where
    extraConstructor l r = M.ExtraConstructor l r


{-

TODO: so, for functions, it looks like we need:
-- the datatypes carry UIDs and type information, imposed by the TTF encodings (which would ask for the function for enforcement)
-- Quantities would likely need to become typed
-- ChunkDB change could never have come sooner!
-- in fact, it looks like I need to be able to put arbitrary data into ChunkDBs in order to have typed variables, or else many typed lists will break homogeneity rule!!!!

-}
