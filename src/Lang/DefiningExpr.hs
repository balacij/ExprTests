module Lang.DefiningExpr where

import Lang.GenericClasses
import Lang.Expr
import Lang.Exprs
import Lang.ModelExpr

{------------------------------------------------------------------------------
| Potential expression containers
|
| ( Mostly not important, done purely for learning and experimentation. )
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

instance HasUID (ExprBox et) where
    uid = _ebName

instance HasUID (ModelExprBox et) where
    uid = _mebName

instance HasUID (EMExprBox et t) where
    uid = _emName

{------------------------------------------------------------------------------
| We need a very generic typeclass to represent "grabbing typed expressions from things"
| (similar to existing "DefiningExpr" typeclass in Drasil) 
|
| We want it to be generic because "definition"s should be able to swap out the language
| used to define them. Through later restricting the languages used, we can have clusters
| of definitions by Exprs, ModelExprs, Literals (potential move from literals to new
| a new Literals-only language), etc.
------------------------------------------------------------------------------}

{-~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
~ First attempt at creating the typeclass didn't pan out so well..
~
~ Check out the code first!
~ 
~ Since the `ek` didn't appear on the input of `defnExpr`, it was causing ambiguous type errors in usage!
~ Those errors resulted in us requiring a __manually written monomorphic type signature for all usages__!
~ I don't think this would work out too well for us, but I think it's suboptimal because we might need to
~ often enable AllowAmbiguousTypes to make things work (though I'll need to double check later).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

class DefiningExpr t ek et where
    defnExpr :: t et -> ek et

instance DefiningExpr ExprBox Expr t where
    defnExpr = _ebExpr

instance DefiningExpr ModelExprBox ModelExpr t where
    defnExpr = _mebModelExpr

instance DefiningExpr (EMExprBox et) et t where
    defnExpr = _emExpr
-}


{-~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
~ Second attempt works a lot better because we also require the inputted thing to contain
~ but its not without its caveats...
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-}
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

{-~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
~ Third attempt, using something ~similar to the current one:
~ 
~ class DefiningExpr c where
~   -- | Provides a 'Lens' to the expression.
~   defnExpr :: Express e => Lens' (c e) e
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-}

-- with this one, we would have the expr type directly with the expr kind
class DefiningExpr3 t e where -- This is just the 'getter' part of the Lens. We don't often seem to use the 'setter' part, but it shouldn't be breaking anything here.
    defnExpr3 :: t e -> e

-- Unfortunately, this original style is incompatible with with the alternative styles for expr-containers
-- 
-- instance DefiningExpr3 (EMExprBox et) e where
--     defnExpr3 = _emExpr
--
-- so,
--
-- we need a new box where the type is purely up to the writer
data ExprBox' et = ExprBox' {
        _ebExpr' :: et,
        _ebName' :: String
    }

instance DefiningExpr3 ExprBox' e where
    defnExpr3 = _ebExpr'

instance HasUID (ExprBox' et) where
    uid = _ebName'



-- | Basic test using the 2nd style of boxes
p :: EMExprBox Expr Integer
p = EMExprBox {
        _emExpr = int 1,
        _emName = "integers"
    }

q :: String
q = eToStr $ defnExpr2 p

r :: String
r = meToStr $ express $ defnExpr2 p

-- | Basic test using the 3rd/original style of boxes
p' :: ExprBox' (Expr Integer)
p' = ExprBox' {
        _ebExpr' = int 1,
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

-- Let's use the ttf constructors to continue demoing our common functions
p'' :: ExprC r => ExprBox' (r Integer)
p'' = ExprBox' {
        _ebExpr' = int 1,
        _ebName' = "p''"
    }

r''' :: String
r''' = thirdF p''
-- ..it looks like they work well.

{-~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
~ Conclusion: DefiningExpr3 (the original style) is the best working one.
~
~ TODO: Justify.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-}
