module Theory.GeneralDefinition where

import Theory.ModelKinds

-- TODO: general usage

data GeneralDefinition where
    GD :: String -> AbstractModelKind -> GeneralDefinition

gd4 :: GeneralDefinition
gd4 = GD "gd4" amk4

