module Lang.GenericClasses where

{------------------------------------------------------------------------------
| An interface to get pseudo-UIDs.
------------------------------------------------------------------------------}
class HasUID a where
    uid :: a -> String
