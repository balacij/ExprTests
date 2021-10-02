module Lang.GenericClasses where

-- | An interface to get the UID of a chunk.
class HasUID a where
    uid :: a -> String

-- | An interface to get the "short name" of a chunk.
class HasShortName a where
    shrtName :: a -> String
