module Lang.QDefinition where

import Prelude hiding (concat)

import Lang.DefiningExpr
import Lang.Display
import Lang.Expr
import Lang.Exprs
import Lang.ModelExpr
import Lang.GenericClasses

import Data.Maybe (mapMaybe)
import qualified Data.Map as M
import Lang.GenericClasses (HasShortName)


-- TODO: ASIDE: Should QDefinitions be moved from drasil-lang to drasil-theory?


-- | Standard QDefinitions (except it's missing the quantity it defines).
data QDefinition e where
    -- | Essentially the same, without UIDs and chunks from the original Drasil code
    --   the second String is so that we can implement the "Short Name" typeclass
    --   (they're usually derived from the "quantity", but we don't have those here).
    QD :: String -> String -> e -> QDefinition e

instance DefiningExpr3 QDefinition e where
    defnExpr3 (QD _ _ exp) = exp

instance HasShortName (QDefinition e) where
    shrtName (QD _ shrt _) = shrt

instance HasUID (QDefinition e) where
    uid (QD n _ _) = n

instance Display (QDefinition (Expr t)) where
    display (QD _ _ e) = E e

instance Display (QDefinition (ModelExpr t)) where
    display (QD _ _ me) = ME me

{- basic examples -}

qd1 :: QDefinition (Expr Integer)
qd1 = QD "qd1" "shrtname" $ int 1

-- qd2 :: QDefinition (Expr Integer)
qd2 :: ExprC r => QDefinition (r Integer)  -- either of these type signatures would work
qd2 = QD "qd2" "shrtname" $ int 1 `add` int 3


{-~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
~ With untyped expressions, we would hide "QDefinition Exprs" using
~ a type synonym to write "SimpleQDef" instead.
~
~ Are we able to create a nice type synonym for "QDefinitions (Expr x)"s?
~
~ Approx. Goal: https://github.com/JacquesCarette/Drasil/blob/moveDerivToModelExpr/code/drasil-lang/lib/Language/Drasil/Synonyms.hs
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-}

-- It looks like we need to add the expression's resultant type as a param
type SimpleQDef a = QDefinition (Expr a)

qd3 :: SimpleQDef Integer
qd3 = QD "qd3" "shrtname" $ int 1

-- but we can also hide it in a wrapper type.. The problem with this is that we lose "a" from everywhere
-- would we be able to resolve that with Typeable usage? Seems bad to do, but it might work.
data SimpleQDef' = forall a. SimpleQDef' (QDefinition (Expr a))
-- It's ok, but it doesn't work very well with the typeclasses defined above. We either need alternative variants or to restrict
-- usage of this to just the examples, to make them more readable?

qd3' :: SimpleQDef'
qd3' = SimpleQDef' $ QD "qd3'" "shrtname" $ int 1

-- here it works, but it's trivial
qd3't :: String
qd3't = case qd3' of {
            SimpleQDef' qd -> meToStr $ express $ defnExpr3 qd
        }

qd4 :: ExprC r => QDefinition (r String)
qd4 = QD "qd4" "shrtname" $ concat (str "q") (str "d4")

qd4' :: SimpleQDef'
qd4' = SimpleQDef' $ QD "qd4'" "shrtname" $ concat (str "q") (str "d4'")


{-  Here's an attempt at creating an alternative version of Express that works better with SimpleQDef', but it failed.

class Express' a where
    express' :: a -> ModelExpr t  -- the difference between "express" and this is that "a" is not also given type param. arg. "t"

instance Express' SimpleQDef' where
    express' (SimpleQDef' qd) = express $ defnExpr3 qd  -- this "defnExpr3 qd" causes a seemingly unresolvable typing issue!

Let's unpack it here:

    Consider the following code snippet:
    
    ```
        express' (SimpleQDef' qd) = express $ defnExpr3 qd
    ```

    GHC appropriately resolves the type signature `express' :: SimpleQDef' -> p`, and gives the following error for the line above:
    ```
        • Couldn't match expected type ‘p’ with actual type ‘ModelExpr a’
    ```

    However, if we NEVER explicitly write an explicit polymorphic type signature (I'm not sure how else to explain this phenomena),
    then we can write fully monomorphic things without issue:

    ```
        simpleQDef'ToStr (SimpleQDef' qd) = meToStr $ express $ defnExpr3 qd
    ```

    as we might see below:
-}

simpleQDef'ToStr :: SimpleQDef' -> String
simpleQDef'ToStr (SimpleQDef' qd) = meToStr $ express $ defnExpr3 qd


{-~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
~ Would this alternative wrapper type be friendly to map usage?
~
~ (Not that we would need a "QDefinition" map though!)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-}

simpleQDef'Map :: M.Map String SimpleQDef'
simpleQDef'Map = M.insert "qd4'" qd4' $ M.singleton "qd3'" qd3'

searchedQd4'Str :: String
searchedQd4'Str = maybe
    (error "somehow didn't find it in map")
    simpleQDef'ToStr
    -- (\(SimpleQDef' qd) -> meToStr $ express $ defnExpr3 qd) -- writing this is okay and works, but as soon as we try to pass around the qd, it can become buggy unless the accepting function accepts every type variable as input
    $ M.lookup "qd4'" simpleQDef'Map

-- TODO: More map-related functions should be reconstructed
