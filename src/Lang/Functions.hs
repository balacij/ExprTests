{-# LANGUAGE TypeApplications #-}
module Lang.Functions where
import Lang.GenericClasses
import Lang.Expr
import Lang.Exprs

data FunctionPkt = FunctionPkt {
      _uid          :: UID
    , _description  :: String   -- in practice, `Sentence`
    , _symbol       :: String   -- in practice, `Symbol`
    , _inputSymbols :: [String] -- in practice, `Vector Symbol` because we will frequently index by a number, so it's best to use a datatype with O(1) indexing    
    -- , etc ...
}

-- NOTE: I'm making a design choice: named function parameters should not be available in the "top-level" Expr language. They should only be
--       be made available to "CodeFunctions" (for GOOL) which would check calls at the "drasil runtime" (as opposed to these, which could be
--       checked at the "Drasil Haskell compilation"-time). We might require Dependent Types if we want to enforce type restraints
--       on the "named" arguments (in other words, core Haskell might be insufficient here).

data Function e ts where
    NullaryFunction  :: FunctionPkt -> e t                        -> Function e (e t)
    UnaryFunction    :: FunctionPkt -> (e i -> e o)               -> Function e (e i -> e o)
    BinaryFunction   :: FunctionPkt -> (e l -> e r -> e t)        -> Function e (e l -> e r -> e t)
    TertiaryFunction :: FunctionPkt -> (e a -> e b -> e c -> e t) -> Function e (e a -> e b -> e c -> e t)
    -- ... can we collapse this?

class ExprFuncs (r :: * -> *) where
    callNullary  :: Function r (r t) -> r t
    callUnary    :: Function r (r i -> r o) -> r i -> r o
    callBinary   :: Function r (r il -> r ir -> r t) -> r il -> r ir -> r t
    callTertiary :: Function r (r a -> r b -> r c -> r t) -> r a -> r b -> r c -> r t

-- Well, this would only work well for purely functional languages.
inlinedUnaryCall :: Function Expr (Expr l -> Expr t) -> Expr l -> Expr t
inlinedUnaryCall (UnaryFunction pkt f) = f


-- maybe we can collapse the definitions, but then restrict allowed "functions" via smart constructors? Probably the best choice!

-- TODO: uh oh! This is basically QDefinitions! :)
data Function' ts where
    -- realistically, I should be unrolling the `FunctionPkt`, but it's not a problem for this prototype
    Function' :: FunctionPkt -> ts -> Function' ts

-- TODO: I guess I just found the real type constructors we need to form proper function definitions :)

-- We can probably add a constraint on `t` that it must be a literal, but i'm not quite sure what good that would do...
mkNullaryFunc :: FunctionPkt -> r t -> Function' (r t)
mkNullaryFunc = Function'

mkUnaryFunc :: FunctionPkt -> (r i -> r o) -> Function' (r i -> r o)
mkUnaryFunc = Function'

mkBinaryFunc :: FunctionPkt -> (r il -> r ir -> r t) -> Function' (r il -> r ir -> r t)
mkBinaryFunc = Function'
-- etc...

func' :: ExprC r => Function' (r Integer -> r Integer)
func' = mkUnaryFunc (FunctionPkt "some function" "n/a" "f" ["x"]) (\x -> add x (int 1))

-- For what things would this be "code generatable"?
class CanGenCode t where
    genCode :: t -> String -- we'll need to pretend the "String" is a "CodeChunk" (not a CodeExpr!)

instance CanGenCode (Function' (Expr t)) where
    genCode _ = "nullary"
instance CanGenCode (Function' (Expr i -> Expr o)) where
    genCode _ = "unary"
