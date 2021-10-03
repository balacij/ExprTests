module Theory.ModelKinds where

import Lang.Display
import Lang.Expr
import Lang.Exprs
import Lang.GenericClasses
import Lang.ModelExpr
import Lang.QDefinition

import Lang.RelationConcept

{--------------------------------------------------------------------------------
| First attempt at ModelKinds worked but it still wasn't very good.
---------------------------------------------------------------------------------

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

{--------------------------------------------------------------------------------
| Second attempt at ModelKinds...
|
| It works, but it's still not good because the type variables don't always make
| apply to each situation. I think we can still do better.
--------------------------------------------------------------------------------}

-- | Is a model "concrete" (must be usable in code generation), 
--   or "abstract" (can be, but not needed to, be used in code generation,
--   but must be usable in modelling)?
data Abstractness = Concrete | Abstract

-- | `ModelKinds` is a container for describing allowed "models" inside of TMs/IMs/GDs.
--   
--   There are different types of them. Some may be "concrete" or "abstract",
--   and some would contain Exprs or ModelExprs or other usable types.
data ModelKinds (c :: Abstractness) e where
    EquationalModel :: QDefinition e     -> ModelKinds c e
    -- TODO: EquationalRealm (using ConstraintSet replica)
    -- TODO: EquationalConstraints (using MultiDefn replica)
    DEModel         :: RelationConcept Relation -> ModelKinds Concrete e
    OtherModel      :: RelationConcept e -> ModelKinds Abstract e

-- | Container for "Concrete Model Kinds usable in code generation"
data ConcreteModelKind = forall t. CMK (ModelKinds Concrete (Expr t))
-- | Container for "Abstract Model Kinds"
data AbstractModelKind = forall t. AMK (ModelKinds Abstract (ModelExpr t))

{-
   Note that this doesn't stop us from creating "ModelKinds Concrete (ModelExpr t)", which would
   be inadmissible. This is a rather brittle solution, with an unnecessary "Expr"-kind and
   "Abstractness" imposition.

   To make up for allowing inadmissible constructions, we are forced to force requirements in
   the smart constructors.
-}

-- | QDefinitions with Exprs should only be used to create "Concrete Model Kinds"
conEquatModel :: QDefinition (Expr t) -> ConcreteModelKind
conEquatModel = CMK . EquationalModel

-- | QDefinitions with ModelExprs should only be used to create "Abstract Model Kinds"
absEquatModel :: QDefinition (ModelExpr t) -> AbstractModelKind
absEquatModel = AMK . EquationalModel

-- | Differential Models must be concrete??? Well, it seems we need another variant as well for abstract...
deModel :: RelationConcept Relation -> ConcreteModelKind
deModel = CMK . DEModel

othModel :: RelationConcept (ModelExpr e) -> AbstractModelKind
othModel = AMK . OtherModel

{- some basic examples of usage -}
cmk1 :: ConcreteModelKind
cmk1 = conEquatModel qd1

cmk4 :: ConcreteModelKind
cmk4 = conEquatModel qd4

amk4 :: AbstractModelKind
amk4 = absEquatModel qd4

-- The current ModelKinds code has a heavy amount of duplicate code.
-- So, I think that we should try to re-evaluate it. I think that we
-- should try analyzing the constraints that we use on ModelKinds,
-- and then try to make "ModelKinds" a type alias for arbitrary types that
-- satisfy those constraints. This could be done similar to as we did in
-- SubtypeLike.hs.

{--------------------------------------------------------------------------------
| Third attempt at ModelKinds, going for a very slim variant.
|
| I think this is the most modular & extensible variant, imposing the
| implicit restrictions explicitly. With this design, we designate what we need
| from a "model" for our systems to handle. These 2 variants "ModelKinds2" and
| "InstantiableModelKinds2" would be the 2 "implementations" of "CoreModelKinds2"
| (which exposes a type variable so as to be further extensible) which are
| specifically intended for our style of SRS documents and other generators/printers.
| However, if we wanted to extend it for some new variant, we can do so easily
| by making those new "ModelKinds" variants build off of "CoreModelKinds2",
| imposing the same restrictions needed for the Drasil core-libs to be compatible. 
--------------------------------------------------------------------------------}

-- Do we want it as a type? No, I think this might be a bit problematic.
type CoreModelKinds2 e = (
      Display e
    , HasUID e
    , HasShortName e
    -- note: There will likely need to be at least 6 restrictions, but I doubt those extra ones will be problematic.
    --       these ones we have here should be the minimum to showing how this would be a robust solution.
    ) => e

-- I think a typeclass will work better.
class (Display e
    ,  HasUID e
    ,  HasShortName e
    ) => CoreModelKinds2' e where

{- Versions of the "CoreModelKinds2'" that use new data types -}

data ModelKinds2 = forall e. CoreModelKinds2' e => ModelKinds2 e

data InstantiableModelKinds2 = forall e. (
      -- TODO: constraint to restrict types to only ones that can we can directly write usable code expressions.
      --       There's a few ways we can do this, but I'm wondering if we should be moving some components of drasil-code
      --       to drasil-lang first instead. This part still requires design.
    ) => InstantiableModelKinds2 (CoreModelKinds2 e)

{- Alternatively, we can go down the route of having it as a typeclass too! -}

class (CoreModelKinds2' e
      -- Here is where the "code usable" restriction would need to go
      ) => InstantiableModelKinds2' e where

-- NOTE: we cannot create a new typeclass for "ModelKinds2" to mask the "e" parameter of the "CoreModelKinds2'"
-- but that's fine!

{- TODO: ODDITY:
    "InstantiableModelKinds" is an oddity. It's an extension of ModelKinds that allows for the Drasil systems to recognize that
    this particular model can (and should) be used in code generation.

    Does this mean that this variant of ModelKinds should reside in `drasil-code`? Does this mean that `drasil-code-base`'s CodeExpr
    should reside in `drasil-lang`?

    Well, at the very least, depending on how we important we view modularity here, it can be a problem.
-}

-- Instantiating CoreModelKinds2' for Simple QDefinitions
instance CoreModelKinds2' (QDefinition (Expr t)) where
-- Instantiating CoreModelKinds2' for Model QDefinitions
instance CoreModelKinds2' (QDefinition (ModelExpr t)) where
-- Instantiating InstantiableCoreModelKinds2' for Simple QDefinitions
instance InstantiableModelKinds2' (QDefinition (Expr t)) where
    -- TODO: we would likely need to fill something in here, which is why we won't be able to instantiate this for ModelExpr.

{-

Further commentary:

Pros:
    - Modular + Extensible
    - No duplicate code
    - Type variables make sense (pro against the previous 2 gens of ModelKinds)
        - No "Abstractness (Concrete/Abstract)"
        - No forcing things to use "Exprs" or "ModelExprs" at all! This should help a lot for some mathematical models
          which wouldn't be always be modelled by "expressions" but by, perhaps, coefficients, etc.
    - With this variant, no smart constructors are really needed because it's just a series of constraints
      imposed on things. Though, having a small simple smart constructor doesn't hurt, I just don't see how
      it could be used(?).

Cons:
    - We lose the type tags (though, we didn't use them yet, and I'm not sure if we intended to). However,
      these type tags can be re-made in the Drasil-level instead of Haskell-level.
    - Despite us never having ModelKinds constructors that had more than 1 input requirement, this would make
      those unknown instances require a wrapper to contain/merge those 2 data types instead of having it done
      here.

-}
