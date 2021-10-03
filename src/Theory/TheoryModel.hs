module Theory.TheoryModel where

import Theory.ModelKinds

-- TODO: general usage

data TheoryModel where
    TM :: String -> AbstractModelKind -> TheoryModel
    -- TODO: create the same 2 variants as InstanceModel using the new ModelKinds

tm4 :: TheoryModel
tm4 = TM "tm4" amk4

