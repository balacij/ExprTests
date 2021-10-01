module Lang.RelationConcept where

import Lang.ModelExpr

data RelationConcept e where -- TODO: Rename?
    RC :: String -> e Bool -> RelationConcept (e Bool)

type Relation = ModelExpr Bool

-- TODO: general usage
