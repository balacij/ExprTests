module Lang.Display where

import Lang.Expr
import Lang.ModelExpr

-- | A basic display language.
--   
--   For new "ModelKinds", this would be more usable for them to direct information without being bound
--   by the strict typing required for Exprs or ModelExprs.
data DisplayLang where
    -- | Expr terms.
    E :: Expr a -> DisplayLang

    -- | ModelExpr terms.
    ME :: ModelExpr a -> DisplayLang

    -- Potential extra terms:
    --   1. "multiple, unrelated exprs (of any kind) for display"
    --   2. ?


-- | An interface to allow things to be brought into the "Display" language.
class Display t where
    display :: t -> DisplayLang

-- | Basic implementation for all Exprs to be "display"ed.
instance Display (Expr a) where
    display = E

-- | Basic implementation for all ModelExprs to be "display"ed.
instance Display (ModelExpr a) where
    display = ME
