{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Lib where

import Prelude hiding (concat)

import qualified Data.Map as M

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
class DefiningExpr2 t ek et where
    defnExpr2 :: t ek et -> ek et

-- Unfortunately, `ExprBox` and `ModelExprBox` won't work here!
-- They both have a direct Expr reference in their data type but not in their type variables
-- (where we rely on for that information to make the `DefiningExpr2` typeclass generic).
-- 
-- instance DefiningExpr2 ExprBox Expr t where
--     defnExpr2 = _

instance DefiningExpr2 EMExprBox et t where
    defnExpr2 = _emExpr

{-
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
~ Third attempt, using something ~similar to the current one:

class DefiningExpr c where
  -- | Provides a 'Lens' to the expression.
  defnExpr :: Express e => Lens' (c e) e
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-}

-- with this one, we would have the expr type directly with the expr kind
class DefiningExpr3 t e where -- This is just the 'getter' part of the Lens. We don't often seem to use the 'setter' part, but it shouldn't be breaking anything here.
    defnExpr3 :: t e -> e

-- Unfortunately, this original style is incompatible with with the alternative styles for expr-containers
-- 
-- instance DefiningExpr3 (EMExprBox et) e where
--     defnExpr3 = _emExpr
--
-- so,

-- we need a new box where the type is purely up to the writer
data ExprBox' et = ExprBox' {
        _ebExpr' :: et,
        _ebName' :: String
    }

instance DefiningExpr3 ExprBox' e where
  defnExpr3 = _ebExpr'

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
q = eToStr $ defnExpr2 p

r :: String
r = meToStr $ express $ defnExpr2 p

-- basic tests using the 3rd/original style of boxes
p' :: ExprBox' (Expr Integer)
p' = ExprBox' {
        _ebExpr' = Int 1,
        _ebName' = "p'"
    }

q' :: String
q' = eToStr $ defnExpr3 p'

r' :: String
r' = meToStr $ express $ defnExpr3 p'

-- Let's make sure that we can still write nice functions on these
thirdF :: ExprBox' (Expr a) -> String
thirdF eb' = meToStr $ express $ defnExpr3 eb'

r'' :: String
r'' = thirdF p'

{------------------------------------------------------------------------------
| TTF smart constructors
------------------------------------------------------------------------------}
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


{------------------------------------------------------------------------------
| Now, we should try to model basic versions of QDs, TMs, IMs, etc
------------------------------------------------------------------------------}

data QDefinition e where
    -- Essentially the same, without UIDs and chunks from the original Drasil code
    QD :: String -> e -> QDefinition e

instance DefiningExpr3 QDefinition e where
    defnExpr3 (QD _ exp) = exp

qd1 :: QDefinition (Expr Integer)
qd1 = QD "qd1" $ Int 1

-- qd2 :: QDefinition (Expr Integer)
qd2 :: ExprC r => QDefinition (r Integer)  -- either of these type signatures would work
qd2 = QD "qd2" $ int 1

{-
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
~ With untyped expressions, we would hide "QDefinition Exprs" using
~ a type synonym to write "SimpleQDef" instead.
~
~ Are we able to create a nice type synonym for "QDefinitions (Expr x)"s?
~
~ Approx. Goal: https://github.com/JacquesCarette/Drasil/blob/moveDerivToModelExpr/code/drasil-lang/lib/Language/Drasil/Synonyms.hs
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-}

-- It looks like we need to add the expression's resultant type as a param
type SimpleQDef a = QDefinition (Expr a)

qd3 :: SimpleQDef Integer
qd3 = QD "qd3" $ int 1

-- but we can hide it in a wrapper type? The problem with this is that we lose "a" from everywhere
-- would we be able to resolve that with Typeable usage? Seems bad to do, but it might work.
data SimpleQDef' = forall a. SimpleQDef' (QDefinition (Expr a))
-- It's ok, but it doesn't work very well with the typeclasses defined above. We'll need alternative variants...

qd3' :: SimpleQDef'
qd3' = SimpleQDef' $ QD "qd3'" $ int 1

-- here it works, but it's trivial
qd3't :: String
qd3't = case qd3' of {
            SimpleQDef' qd -> meToStr $ express $ defnExpr3 qd
        }

qd4' :: SimpleQDef'
qd4' = SimpleQDef' $ QD "qd4'" $ concat (str "q") (str "d4'")


{-  Here's an attempt at creating an alternative version of Express that works better with SimpleQDef', but it failed.

class Express' a where
    express' :: a -> ModelExpr t  -- the difference between "express" and this is that "a" is not also given type param. arg. "t"

instance Express' SimpleQDef' where
    express' (SimpleQDef' qd) = express $ defnExpr3 qd  -- this "defnExpr3 qd" causes a seemingly unresolvable typing issue!

Let's unpack it here:

    Consider the following code snippet:
    
    ```
        express' (SimpleQDef' qd) = express $ defnExpr3 qd
    ```

    GHC recognizes appropriately resolves the type signature `express' :: SimpleQDef' -> p`, and gives the following error for the line above:
    ```
        • Couldn't match expected type ‘p’ with actual type ‘ModelExpr a’
    ```

    However, if we NEVER explicitly write an explicit polymorphic type signature (I'm not sure how else to explain this phenomena),
    then we can write fully monomorphic things without issue:

    ```
        simpleQDef'ToStr (SimpleQDef' qd) = meToStr $ express $ defnExpr3 qd
    ```

    as we might see below:
-}

simpleQDef'ToStr :: SimpleQDef' -> String
simpleQDef'ToStr (SimpleQDef' qd) = meToStr $ express $ defnExpr3 qd


{-
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
~ Would this alternative wrapper type be friendly to map usage?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-}

simpleQDef'Map :: M.Map String SimpleQDef'
simpleQDef'Map = M.insert "qd4'" qd4' $ M.singleton "qd3'" qd3'

searchedQd4'Str :: String
searchedQd4'Str = maybe
    (error "somehow didn't find it in map")
    (\(SimpleQDef' qd) -> meToStr $ express $ defnExpr3 qd) -- writing this is okay and works, but as soon as we try to pass around the qd, it can become buggy
    $ M.lookup "qd4'" simpleQDef'Map

-- TODO: More map-related functions should be reconstructed

-- TODO: DataDefinitions replica
-- TODO: ConstraintSet replica
-- TODO: "Function Definition" variant of QDefinitions replica
-- TODO: ModelKinds replica
-- TODO: IMs replica & general usage
-- TODO: TMs replica & general usage
-- TODO: GDs replica & general usage
-- TODO: SystemInformation replica & general usage

{------------------------------------------------------------------------------
| Standard Stack template code below
------------------------------------------------------------------------------}

someFunc :: IO ()
someFunc = putStrLn "someFunc"
