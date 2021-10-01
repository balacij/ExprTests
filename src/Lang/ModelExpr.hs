module Lang.ModelExpr where

import Lang.Expr (binParen)
import qualified Lang.Expr as E (Expr(..))

-- | The extended "model"/"display" variant of the "Expr" language (with 1 extra constructor)
data ModelExpr a where
    Int :: Integer -> ModelExpr Integer
    Str :: String -> ModelExpr String
    Add :: Num a => ModelExpr a -> ModelExpr a -> ModelExpr a
    Concat :: ModelExpr String -> ModelExpr String -> ModelExpr String
    Eq  :: Eq a => ModelExpr a -> ModelExpr a -> ModelExpr Bool
    ExtraConstructor :: Show a => ModelExpr a -> ModelExpr a -> ModelExpr a

-- | Render `ModelExpr`s as readable Strings
meToStr :: ModelExpr a -> String
meToStr (Int n) = show n
meToStr (Str s) = '"':s ++ "\""
meToStr (Add l r) = binParen "+" (meToStr l) (meToStr r)
meToStr (Concat l r) = binParen "++" (meToStr l) (meToStr r)
meToStr (Eq l r) = binParen "==" (meToStr l) (meToStr r)
meToStr (ExtraConstructor l r) = binParen "=>" (meToStr l) (meToStr r)


{------------------------------------------------------------------------------
| An interface to "express" things using the model language, with a necessary
| type information passthrough too.
------------------------------------------------------------------------------}

-- | Express takes things with 1 type variable, and expects them to be able to
--   be converted into `ModelExpr` language with the same type variable.
class Express a t where
    express :: a t -> ModelExpr t

-- | Expressing Expr in ModelExpr is essentially reconstruction
instance Express E.Expr t where
    express (E.Int n)      = Int n
    express (E.Str s)      = Str s
    express (E.Add l r)    = Add (express l) (express r)
    express (E.Concat l r) = Concat (express l) (express r)
    express (E.Eq l r)     = Eq (express l) (express r)

-- | ModelExprs can also be expressed as themselves
instance Express ModelExpr t where
    express = id

{-~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
~ One of the bad things about "Express" is that it has 2 type variables.
~ It would be nice to be able to "express" anything, arbitrarily.
~
~ See related discussion on QDefinition.hs too.
~
~ TODO: Figure out if we can get rid of type var "t" from Express.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-}
