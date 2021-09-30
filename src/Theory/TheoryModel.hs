module Theory.TheoryModel where

import Theory.ModelKinds

-- TODO: general usage

data TheoryModel where
    TM :: String -> AbstractModelKind -> TheoryModel

tm4 :: TheoryModel
tm4 = TM "tm4" amk4

