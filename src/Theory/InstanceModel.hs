module Theory.InstanceModel where

import Theory.ModelKinds

-- TODO: general usage

data InstanceModel where
    IM :: String -> ConcreteModelKind -> InstanceModel

im1 :: InstanceModel
im1 = IM "im1" cmk1

im4 :: InstanceModel
im4 = IM "im4" cmk4
