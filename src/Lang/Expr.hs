module Lang.Expr where

-- | The standard expression language
data Expr a where
    Int :: Integer -> Expr Integer
    Str :: String -> Expr String
    Add :: Num a => Expr a -> Expr a -> Expr a
    Concat :: Expr String -> Expr String -> Expr String
    Eq  :: Eq a => Expr a -> Expr a -> Expr Bool

-- | Basic evaluation (for `Expr` only!)
eval :: Expr a -> a
eval (Int n) = n
eval (Str s) = s
eval (Add l r) = eval l + eval r
eval (Concat h t) = eval h ++ eval t
eval (Eq l r) = eval l == eval r

-- | Helper function for rendering binary operations.
binParen :: String -> String -> String -> String
binParen bin l r = "(" ++ l ++ ' ':bin ++ ' ':r ++ ")"

-- TODO: Realistically, should we ever be directly displaying Exprs? Or should we be upgrading to ModelExprs first, and then rendering? I think the latter...
-- | Render `Expr`s as readable Strings
eToStr :: Expr a -> String
eToStr (Int n) = show n
eToStr (Str s) = '"':s ++ "\""
eToStr (Add l r) = binParen "+" (eToStr l) (eToStr r)
eToStr (Eq l r) = binParen "==" (eToStr l) (eToStr r)
eToStr (Concat l r) = binParen "++" (eToStr l) (eToStr r)
