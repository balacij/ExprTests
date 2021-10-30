module Lang.GenericClasses where

type UID = String

-- | An interface to get the UID of a chunk.
class HasUID a where
    uid :: a -> UID

-- | An interface to get the "short name" of a chunk.
class HasShortName a where
    shrtName :: a -> String

class DrasilDumpable a where
    dump :: a -> String
