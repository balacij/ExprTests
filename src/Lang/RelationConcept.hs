module Lang.RelationConcept where

import Lang.ModelExpr
import Lang.GenericClasses

data RelationConcept e where -- TODO: Rename?
    RC :: String -> e Bool -> RelationConcept (e Bool)

type Relation = ModelExpr Bool

instance HasUID (RelationConcept e) where
    uid (RC s _) = s
