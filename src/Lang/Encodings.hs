module Lang.Encodings where

-- | Keeping it simple, we can have 1 single thing which describes all things Drasil encodings should do, in some capacity or another
class DrasilPrinter i o where
    drasilPrint :: i -> o              -- not using 'print' to avoid a conflict with Prelude.print...

-- This class is quite weird. We should think of it's semantic importance to the theory of Drasil instead of just "can I get an X from Y?".
