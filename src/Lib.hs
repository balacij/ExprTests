{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Lib where

{------------------------------------------------------------------------------
| Two basic expression languages
------------------------------------------------------------------------------}

-- The standard expression language
data Expr a where
    Int :: Integer -> Expr Integer
    Str :: String -> Expr String
    Add :: Num a => Expr a -> Expr a -> Expr a
    Concat :: Expr String -> Expr String -> Expr String

-- The "model"/"display" variant (with 1 extra constructor)
data ModelExpr a where
    Int' :: Integer -> ModelExpr Integer
    Str' :: String -> ModelExpr String
    Add' :: Num a => ModelExpr a -> ModelExpr a -> ModelExpr a
    Concat' :: ModelExpr String -> ModelExpr String -> ModelExpr String
    ExtraConstructor :: Show a => ModelExpr a -> ModelExpr a -> ModelExpr a

{------------------------------------------------------------------------------
| An interface to "express" things using the model language, with a necessary
| type information passthrough too.
------------------------------------------------------------------------------}
class Express a t where
    express :: a t -> ModelExpr t

-- Expressing Expr in ModelExpr is essentially reconstruction
instance Express Expr t where
    express (Int n)      = Int' n
    express (Str s)      = Str' s
    express (Add l r)    = Add' (express l) (express r)
    express (Concat l r) = Concat' (express l) (express r)

-- ModelExprs can also be expressed as themselves
instance Express ModelExpr t where
    express = id

{------------------------------------------------------------------------------
| Potential alternative expression containers
------------------------------------------------------------------------------}

-- We might have an Expr container -- note that the Expr type is pushed to the type variable of the container
-- This might be more similar to some data types related to ModelKinds
data ExprBox et = ExprBox {
    _ebExpr :: Expr et,
    _ebName :: String
}

-- We might have a ModelExpr container -- note, again, that the ModelExpr type is pushed to the type variable of the container
data ModelExprBox et = ModelExprBox {
    _mebModelExpr :: ModelExpr et,
    _mebName      :: String
}

-- We might have a doubly generic language container -- variable Expr container, and Expr type
data EMExprBox et t = EMExprBox {
    _emExpr :: et t,
    _emName :: String
}

{------------------------------------------------------------------------------
| We need a very generic typeclass to represent "grabbing typed expressions from things"
------------------------------------------------------------------------------}
{-

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
~ First attempt at creating the typeclass didn't pan out so well..
~ Since the `ek` didn't appear on the input of `defnExpr`, it was causing ambiguous type errors in usage!
~ Those errors resulted in us requiring a __manually written monomorphic type signature for all usages__!
~ I don't think this would work out too well for us, but I think it's suboptimal because we might need to
~ often enable AllowAmbiguousTypes to make things work (though I'll need to double check later).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

class DefiningExpr t ek et where
    defnExpr :: t et -> ek et

instance DefiningExpr ExprBox Expr t where
    defnExpr = _ebExpr

instance DefiningExpr ModelExprBox ModelExpr t where
    defnExpr = _mebModelExpr

instance DefiningExpr (EMExprBox et) et t where
    defnExpr = _emExpr
-}

{-
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
~ Second attempt works a lot better because we also require the inputted thing to contain
~ but its not without its caveats...
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-}
class DefiningExpr t ek et where
    defnExpr :: t ek et -> ek et

-- Unfortunately, `ExprBox` and `ModelExprBox` won't work here!
-- They both have a direct Expr reference in their data type but not in their type variables
-- (where we rely on for that information to make the `DefiningExpr` typeclass generic).
-- 
-- instance DefiningExpr ExprBox Expr t where
--     defnExpr = _

instance DefiningExpr EMExprBox et t where
    defnExpr = _emExpr

{-
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
~ Third attempt, using something ~similar to the current one:

class DefiningExpr c where
  -- | Provides a 'Lens' to the expression.
  defnExpr :: Express e => Lens' (c e) e
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-}

instance DefiningExpr' ExprBox' e where
  defnExpr' = _ebExpr'

-- Unfortunately, this original style is incompatible with with the alternative styles for expr-containers
-- 
-- instance DefiningExpr' (EMExprBox et) e where
--     defnExpr' = _emExpr
--
-- so,

-- we need a new box where the type is purely up to the writer
data ExprBox' et = ExprBox' {
    _ebExpr' :: et,
    _ebName' :: String
}

-- with this one, we would have the expr type directly with the expr kind
class DefiningExpr' t e where -- This is just the 'getter' part of the Lens. We don't often seem to use the 'setter' part, but it shouldn't be breaking anything here.
    defnExpr' :: t e -> e

eval :: Expr a -> a
eval (Int n) = n
eval (Str s) = s
eval (Add l r) = eval l + eval r
eval (Concat h t) = eval h ++ eval t

binParen :: String -> String -> String -> String
binParen bin l r = "(" ++ l ++ ' ':bin ++ ' ':r ++ ")"

eToStr :: Expr a -> String
eToStr (Int n) = show n
eToStr (Str s) = s
eToStr (Add l r) = binParen "+" (eToStr l) (eToStr r)
eToStr (Concat l r) = binParen "++" (eToStr l) (eToStr r)

meToStr :: ModelExpr a -> String
meToStr (Int' n) = show n
meToStr (Str' s) = s
meToStr (Add' l r) = binParen "+" (meToStr l) (meToStr r)
meToStr (Concat' l r) = binParen "++" (meToStr l) (meToStr r)
meToStr (ExtraConstructor l r) = binParen "=>" (meToStr l) (meToStr r)

-- basic tests using the 2nd style of boxes
p :: EMExprBox Expr Integer
p = EMExprBox {
        _emExpr = Int 1,
        _emName = "integers"
    }

q :: String
q = eToStr $ defnExpr p

r :: String
r = meToStr $ express $ defnExpr p

-- basic tests using the 3rd/original style of boxes
p' :: ExprBox' (Expr Integer)
p' = ExprBox' {
        _ebExpr' = Int 1,
        _ebName' = "p'"
    }

q' :: String
q' = eToStr $ defnExpr' p'

r' :: String
r' = meToStr $ express $ defnExpr' p'

-- Let's make sure that we can still write nice functions on these
thirdF :: ExprBox' (Expr a) -> String
thirdF eb' = meToStr $ express $ defnExpr' eb'

r'' :: String
r'' = thirdF p'

-- What about when we want to use functions that accept the TTF constructors?
class ExprC r where
    int :: Integer -> r Integer
    str :: String -> r String
    add :: Num a => r a -> r a -> r a
    concat :: r String -> r String -> r String

class ModelExprC r where
    extraConstructor :: r String -> r String -> r String

instance ExprC Expr where
    int i = Int i
    str s = Str s
    add l r = Add l r 
    concat l r = Concat l r

instance ExprC ModelExpr where
    int i = Int' i
    str s = Str' s
    add l r = Add' l r 
    concat l r = Concat' l r

instance ModelExprC ModelExpr where
    extraConstructor l r = ExtraConstructor l r

-- Let's use the ttf constructors to continue demoing our common functions
p'' :: ExprC r => ExprBox' (r Integer)
p'' = ExprBox' {
        _ebExpr' = int 1,
        _ebName' = "p''"
    }

r''' :: String
r''' = thirdF p''
-- ..it looks like they work well.

someFunc :: IO ()
someFunc = putStrLn "someFunc"
