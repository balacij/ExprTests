{-# LANGUAGE ExistentialQuantification #-}
module SubtypeLike where

-- TODO: NOTE: Subtyping can be "sort of" achieved via:
{--}
data AllLists = forall a. Show a => AllList [a]

instance Show AllLists where
    show (AllList x) = show x

pq :: [AllLists]
pq = [AllList [1,2,3], AllList [True, False, False]]

allPQShown :: [String]
allPQShown = map show pq
{--}
