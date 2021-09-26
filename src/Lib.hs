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
    Eq  :: Eq a => Expr a -> Expr a -> Expr Bool

-- The "model"/"display" variant (with 1 extra constructor)
data ModelExpr a where
    Int' :: Integer -> ModelExpr Integer
    Str' :: String -> ModelExpr String
    Add' :: Num a => ModelExpr a -> ModelExpr a -> ModelExpr a
    Concat' :: ModelExpr String -> ModelExpr String -> ModelExpr String
    Eq'  :: Eq a => ModelExpr a -> ModelExpr a -> ModelExpr Bool
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
    express (Eq l r)     = Eq' (express l) (express r)

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
| An interface to get pseudo-UIDs.
------------------------------------------------------------------------------}
class HasUID a where
    uid :: a -> String

instance HasUID (ExprBox et) where
    uid = _ebName

instance HasUID (ModelExprBox et) where
    uid = _mebName

instance HasUID (EMExprBox et t) where
    uid = _emName

{------------------------------------------------------------------------------
| We need a very generic typeclass to represent "grabbing typed expressions from things"
| (similar to existing "DefiningExpr" typeclass in Drasil) 
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

-- we need a new box where the type is purely up to the writer
data ExprBox' et = ExprBox' {
        _ebExpr' :: et,
        _ebName' :: String
    }

instance DefiningExpr3 ExprBox' e where
    defnExpr3 = _ebExpr'

instance HasUID (ExprBox' et) where
    uid = _ebName'

-- | Basic evaluation (for `Expr` only!)
eval :: Expr a -> a
eval (Int n) = n
eval (Str s) = s
eval (Add l r) = eval l + eval r
eval (Concat h t) = eval h ++ eval t
eval (Eq l r) = eval l == eval r

-- | Helper function for rendering binary operations.
binParen :: String -> String -> String -> String
binParen bin l r = "(" ++ l ++ ' ':bin ++ ' ':r ++ ")"

-- | Render `Expr`s as readable Strings
eToStr :: Expr a -> String
eToStr (Int n) = show n
eToStr (Str s) = '"':s ++ "\""
eToStr (Add l r) = binParen "+" (eToStr l) (eToStr r)
eToStr (Eq l r) = binParen "==" (eToStr l) (eToStr r)
eToStr (Concat l r) = binParen "++" (eToStr l) (eToStr r)

-- TODO: Realistically, should we ever be directly displaying Exprs? Or should we be upgrading to ModelExprs first, and then rendering? I think the latter...

-- | Render `ModelExpr`s as readable Strings
meToStr :: ModelExpr a -> String
meToStr (Int' n) = show n
meToStr (Str' s) = '"':s ++ "\""
meToStr (Add' l r) = binParen "+" (meToStr l) (meToStr r)
meToStr (Concat' l r) = binParen "++" (meToStr l) (meToStr r)
meToStr (Eq' l r) = binParen "==" (meToStr l) (meToStr r)
meToStr (ExtraConstructor l r) = binParen "=>" (meToStr l) (meToStr r)

-- | Basic test using the 2nd style of boxes
p :: EMExprBox Expr Integer
p = EMExprBox {
        _emExpr = Int 1,
        _emName = "integers"
    }

q :: String
q = eToStr $ defnExpr2 p

r :: String
r = meToStr $ express $ defnExpr2 p

-- | Basic test using the 3rd/original style of boxes
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
    eq  :: Eq a => r a -> r a -> r Bool
    concat :: r String -> r String -> r String

class ModelExprC r where
    extraConstructor :: r String -> r String -> r String

instance ExprC Expr where
    int i = Int i
    str s = Str s
    add l r = Add l r
    eq l r = Eq l r
    concat l r = Concat l r

instance ExprC ModelExpr where
    int i = Int' i
    str s = Str' s
    add l r = Add' l r
    eq l r = Eq' l r
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

{-~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
~ QDefinitions first
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-}

data QDefinition e where
    -- Essentially the same, without UIDs and chunks from the original Drasil code
    QD :: String -> e -> QDefinition e

-- TODO: ASIDE: Should QDefinitions be moved from drasil-lang to drasil-theory?

instance DefiningExpr3 QDefinition e where
    defnExpr3 (QD _ exp) = exp

instance HasUID (QDefinition e) where
    uid (QD n _) = n

qd1 :: QDefinition (Expr Integer)
qd1 = QD "qd1" $ Int 1

-- qd2 :: QDefinition (Expr Integer)
qd2 :: ExprC r => QDefinition (r Integer)  -- either of these type signatures would work
qd2 = QD "qd2" $ int 1 `add` int 3

{-~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
~ With untyped expressions, we would hide "QDefinition Exprs" using
~ a type synonym to write "SimpleQDef" instead.
~
~ Are we able to create a nice type synonym for "QDefinitions (Expr x)"s?
~
~ Approx. Goal: https://github.com/JacquesCarette/Drasil/blob/moveDerivToModelExpr/code/drasil-lang/lib/Language/Drasil/Synonyms.hs
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-}

-- It looks like we need to add the expression's resultant type as a param
type SimpleQDef a = QDefinition (Expr a)

qd3 :: SimpleQDef Integer
qd3 = QD "qd3" $ int 1

-- but we can hide it in a wrapper type? The problem with this is that we lose "a" from everywhere
-- would we be able to resolve that with Typeable usage? Seems bad to do, but it might work.
data SimpleQDef' = forall a. SimpleQDef' (QDefinition (Expr a))
-- It's ok, but it doesn't work very well with the typeclasses defined above. We either need alternative variants or to restrict
-- usage of this to just the examples, to make them more readable?

qd3' :: SimpleQDef'
qd3' = SimpleQDef' $ QD "qd3'" $ int 1

-- here it works, but it's trivial
qd3't :: String
qd3't = case qd3' of {
            SimpleQDef' qd -> meToStr $ express $ defnExpr3 qd
        }

qd4 :: ExprC r => QDefinition (r String)
qd4 = QD "qd4" $ concat (str "q") (str "d4")

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

    GHC appropriately resolves the type signature `express' :: SimpleQDef' -> p`, and gives the following error for the line above:
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


{-~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
~ Would this alternative wrapper type be friendly to map usage?
~
~ (Not that we would need a "QDefinition" map though!)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-}

simpleQDef'Map :: M.Map String SimpleQDef'
simpleQDef'Map = M.insert "qd4'" qd4' $ M.singleton "qd3'" qd3'

searchedQd4'Str :: String
searchedQd4'Str = maybe
    (error "somehow didn't find it in map")
    simpleQDef'ToStr
    -- (\(SimpleQDef' qd) -> meToStr $ express $ defnExpr3 qd) -- writing this is okay and works, but as soon as we try to pass around the qd, it can become buggy unless the accepting function accepts every type variable as input
    $ M.lookup "qd4'" simpleQDef'Map

-- TODO: More map-related functions should be reconstructed


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

ddToStr0 :: Express e a => String -> QDefinition (e a) -> String
ddToStr0 n qd = "DataDefinition\nName: " ++ n ++ "\nQD: " ++ qd'
    where qd' = meToStr $ express $ defnExpr3 qd

ddToStr :: DataDefinition -> String
ddToStr (DDE  name qd) = ddToStr0 name qd
ddToStr (DDME name qd) = ddToStr0 name qd

dd1e :: DataDefinition
dd1e = DDE "dd1e" qd2

dd2me :: DataDefinition
dd2me = DDME "dd2me" qd2

type DataDefnMap = M.Map String DataDefinition

dataDefnMap :: M.Map String DataDefinition
dataDefnMap =
      M.insert (uid dd2me) dd2me 
    $ M.singleton (uid dd1e) dd1e

ddTest :: IO ()
ddTest = do
    putStrLn $ ddToStr dd1e
    putStrLn $ maybe "bad match" ddToStr $ M.lookup "dd1e" dataDefnMap
    putStrLn $ maybe "bad match" ddToStr $ M.lookup "dd2me" dataDefnMap

{-~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
~ RelationConcept replica
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-}

data RelationConcept e where -- TODO: Rename?
    RC :: String -> e Bool -> RelationConcept (e Bool)

type Relation = ModelExpr Bool

-- TODO: ConstraintSet replica
-- TODO: MultiDefn replica

{-~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
~ ModelKinds replica & general usage
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-}

-- TODO: ModelKinds replica

-- TODO: explain how these are used
data Concrete
data Abstract

-- TODO: Explain type variables of ModelKinds
data ModelKinds c e where
    EquationalModel :: QDefinition e     -> ModelKinds c e
    -- TODO: EquationalRealm (using ConstraintSet replica)
    -- TODO: EquationalConstraints (using MultiDefn replica)
    DEModel         :: RelationConcept Relation -> ModelKinds Concrete e
    OtherModel      :: RelationConcept e -> ModelKinds Abstract e

-- TODO: Explain type synonyms - https://en.wikibooks.org/wiki/Haskell/Existentially_quantified_types
data ConcreteModelKind = forall t. CMK (ModelKinds Concrete (Expr t))
data AbstractModelKind = forall t. AMK (ModelKinds Abstract (ModelExpr t))

-- TODO: Explain initial smart constructors
-- conEquatModel :: QDefinition (Expr t) -> ModelKinds Concrete (Expr t)
-- conEquatModel = EquationalModel

-- absEquatModel :: QDefinition (ModelExpr t) -> ModelKinds Abstract (ModelExpr t)
-- absEquatModel = EquationalModel

-- deModel :: RelationConcept Relation -> ModelKinds Concrete Relation
-- deModel = DEModel

-- othModel :: RelationConcept (ModelExpr e) -> ModelKinds Abstract (ModelExpr e)
-- othModel = OtherModel

-- TODO: Explain smart constructors
conEquatModel :: QDefinition (Expr t) -> ConcreteModelKind
conEquatModel = CMK . EquationalModel

absEquatModel :: QDefinition (ModelExpr t) -> AbstractModelKind
absEquatModel = AMK . EquationalModel

deModel :: RelationConcept Relation -> ConcreteModelKind
deModel = CMK . DEModel

othModel :: RelationConcept (ModelExpr e) -> AbstractModelKind
othModel = AMK . OtherModel

cmk1 :: ConcreteModelKind
cmk1 = conEquatModel qd1

cmk4 :: ConcreteModelKind
cmk4 = conEquatModel qd4

amk4 :: AbstractModelKind
amk4 = absEquatModel qd4

{-~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
~ InstanceModel replica & general usage
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-}
-- TODO: general usage

data InstanceModel where
    IM :: String -> ConcreteModelKind -> InstanceModel

im1 :: InstanceModel
im1 = IM "im1" cmk1

im4 :: InstanceModel
im4 = IM "im4" cmk4

{-~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
~ TheoryModel replica & general usage
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-}
-- TODO: general usage

data TheoryModel where
    TM :: String -> AbstractModelKind -> TheoryModel

tm4 :: TheoryModel
tm4 = TM "tm4" amk4

{-~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
~ GeneralDefinition replica & general usage
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-}
-- TODO: general usage

data GeneralDefinition where
    GD :: String -> AbstractModelKind -> GeneralDefinition

gd4 :: GeneralDefinition
gd4 = GD "gd4" amk4


{-~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
~ ChunkDB replica & general usage
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-}
-- TODO: ChunkDB replica & general usage


{-~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
~ SystemInformation replica & general usage
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-}
-- TODO: SystemInformation replica & general usage


{------------------------------------------------------------------------------
| Standard Stack template code below
------------------------------------------------------------------------------}

someFunc :: IO ()
someFunc = do
    putStrLn "someFunc"
    putStrLn searchedQd4'Str
    ddTest
