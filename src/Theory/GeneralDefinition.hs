module Theory.GeneralDefinition where

import Theory.ModelKinds

-- TODO: general usage

data GeneralDefinition where
    GD :: String -> AbstractModelKind -> GeneralDefinition
    -- TODO: create the same 2 variants as InstanceModel using the new ModelKinds

gd4 :: GeneralDefinition
gd4 = GD "gd4" amk4

