module Theory.ModelKinds where

import Lang.Expr
import Lang.Exprs
import Lang.ModelExpr
import Lang.QDefinition

import Lang.RelationConcept

{- First attempt at ModelKinds worked but it still wasn't very good.


data Concrete
data Abstract

-- TODO: Explain type variables of ModelKinds
data ModelKinds c e where
    EquationalModel :: QDefinition e     -> ModelKinds c e
    -- TODO: EquationalRealm (using ConstraintSet replica)
    -- TODO: EquationalConstraints (using MultiDefn replica)
    DEModel         :: RelationConcept Relation -> ModelKinds Concrete Relation
    OtherModel      :: RelationConcept e -> ModelKinds Abstract e

-- TODO: Explain type synonym
type ConcreteModelKind = forall t. ModelKinds Concrete (Expr t)
type AbstractModelKind = forall t. ModelKinds Abstract (ModelExpr t)

-- TODO: Explain smart constructors
conEquatModel :: QDefinition (Expr t) -> ModelKinds Concrete (Expr t)
conEquatModel = EquationalModel

absEquatModel :: QDefinition (ModelExpr t) -> ModelKinds Abstract (ModelExpr t)
absEquatModel = EquationalModel

deModel :: RelationConcept Relation -> ModelKinds Concrete Relation
deModel = DEModel

othModel :: RelationConcept (ModelExpr e) -> ModelKinds Abstract (ModelExpr e)
othModel = OtherModel
-}

-- TODO: explain how these are used
data Abstractness = Concrete | Abstract

-- TODO: Explain type variables of ModelKinds
data ModelKinds (c :: Abstractness) e where
    EquationalModel :: QDefinition e     -> ModelKinds c e
    -- TODO: EquationalRealm (using ConstraintSet replica)
    -- TODO: EquationalConstraints (using MultiDefn replica)
    DEModel         :: RelationConcept Relation -> ModelKinds Concrete e
    OtherModel      :: RelationConcept e -> ModelKinds Abstract e

-- TODO: Explain data containers - https://en.wikibooks.org/wiki/Haskell/Existentially_quantified_types
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

-- TODO: Explain smart constructors which use extra containers
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

-- TODO: Figure out what's important about ModelKinds.
-- The current ModelKinds code has a heavy amount of reuse.
-- So, I think that we should try to return it. I think that we
-- should try analyzing the constraints that we use on ModelKinds,
-- and then try to make "ModelKinds" a type alias for arbitrary types that
-- satisfy those constraints. This could be done similar to as we did in
-- SubtypeLike.hs.
