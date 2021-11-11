module Theory.TheoryPresentations where

type Types   = [String]
type Symbols = [String]
type Axioms  = [String]


data Theory = Theory {
      _types   :: Types
    , _symbols :: Symbols
    , _axioms  :: Axioms

    , _title       :: String
    , _description :: String
}
-- a Theory would also have a textual description, etc

class Theoretical a where
    present :: a -> Theory  -- TODO: present?

{-
After looking at what a `TheoryPresentation` is, it sounds like we don't need ModelKinds at all...

a `TheoryPresentation` could be an intended "pushout" of another "theoretical" thing....
  - essentially, instead of "ModelKinds" = Either X/Y/Z/...
    - we have `Theoretical a => a`
    - the `Theory`s would then be usable in SRS generation (this will allow us to automatically gather the nice "Theoretical" things)
    - the "origins" of the Theories will be what we can pull them from (e.g., QDefinitions, MultiDefns, etc)
    - the nice thing about this is that ModelKinds becomes extensible, again, for free! :)
    - the "other things" can still be usable in code generation! They just need to be supported!

-}
