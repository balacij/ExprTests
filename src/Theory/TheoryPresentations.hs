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
