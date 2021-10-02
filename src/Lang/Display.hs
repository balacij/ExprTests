module Lang.Display where

import Lang.Expr
import Lang.ModelExpr

data DisplayLang where
    E :: Expr a -> DisplayLang
    ME :: ModelExpr a -> DisplayLang


class Display t where
    display :: t -> DisplayLang

instance Display (Expr a) where
    display = E

instance Display (ModelExpr a) where
    display = ME
