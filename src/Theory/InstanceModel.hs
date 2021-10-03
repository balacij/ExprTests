{-# OPTIONS_GHC -fprint-potential-instances #-}

module Theory.InstanceModel where

import Prelude hiding (concat)

import Lang.Expr
import Lang.Exprs
import Lang.QDefinition
import Theory.ModelKinds
import Lang.GenericClasses

-- TODO: general usage

data InstanceModel where
    -- | First version of Instance Models
    IM :: String -> ConcreteModelKind -> InstanceModel
    
    -- | Second version of Instance Models (uses 2nd working version of ModelKinds)
    IM2 :: String -> InstantiableModelKinds2 -> InstanceModel

    -- | Third version of Instance Models (uses 2nd' working version of ModelKinds)
    IM2' :: InstantiableModelKinds2' e => String -> e -> InstanceModel

{- IM variants -}

im1 :: InstanceModel
im1 = IM "im1" cmk1

im4 :: InstanceModel
im4 = IM "im4" cmk4

{- IM2 variants -}

im12 :: InstanceModel
im12 = IM2 "im1" $ InstantiableModelKinds2 qd1

im42 :: InstanceModel
im42 = IM2 "im4" $ InstantiableModelKinds2 (qd4 :: QDefinition (Expr String))

{-   If you don't use the manual binding, you will be greeted with:


/home/jasonbalaci/Programming/ExprTests/src/Theory/InstanceModel.hs:28:44: error:
    • Could not deduce (Lang.Exprs.ExprC r0)
        arising from a use of ‘qd4’
      from the context: (Lang.Display.Display (QDefinition (r0 String)),
                         Lang.GenericClasses.HasUID (QDefinition (r0 String)),
                         Lang.GenericClasses.HasShortName (QDefinition (r0 String)))
        bound by a type expected by the context:
                   CoreModelKinds2 (QDefinition (r0 String))
        at src/Theory/InstanceModel.hs:28:44-46
      The type variable ‘r0’ is ambiguous
      These potential instances exist:
        two instances involving out-of-scope types
        (use -fprint-potential-instances to see them all)
    • In the first argument of ‘InstantiableModelKinds2’, namely ‘qd4’
      In the second argument of ‘($)’, namely
        ‘InstantiableModelKinds2 qd4’
      In the expression: IM2 "im4" $ InstantiableModelKinds2 qd4
   |
28 | im42 = IM2 "im4" $ InstantiableModelKinds2 qd4

which makes sense.

The issue appears to be caused because polymorphic "r"'s resolution is ambiguous!
It can go the route of Expr or ModelExpr, without issue. Let's see how we can fix this:
-}

-- Since qd4 used TTF to describe it's expression, it needed to be manually bound,
-- or else it would be ambiguous.
--
-- How can we avoid this manual binding? Possible solutions:
--  1. We can make smart constructors for QDefinitions that force binding to Exprs or ModelExprs
--     depending on which smart constructor is used
--  2. We can force the user to manually write ":: SimpleQDefn String"-like signatures where the "String"
--     is the "Expr"/"ModelExpr" type expression type.
--     This would also require a ":: ModelQDefn String"-like signature too.
--
--  I would tend towards the first solution. Smart constructors are nice for things like this.
--  Additionally, with the first solution, HLS will be able to accurately predict the types 
--  without our manual intervention required for (2).
--
--  Let's see what that is like in practice!
-- ..

{- 1st solution: -}

mkSimpleQD :: String -> String -> Expr t -> QDefinition (Expr t)
mkSimpleQD = QD

-- IMPLICIT TYPE SIGNATURE: qd4HERE :: QDefinition (Expr String)
-- ( I did not write it myself, I used HLS to get it from GHC )

qd4HERE :: QDefinition (Expr String)
qd4HERE = mkSimpleQD "qd4" "shrtname" $ concat (str "q") (str "d4")

im42' :: InstanceModel
im42' = IM2 "im4'" $ InstantiableModelKinds2 qd4HERE -- No need for manual binding!

-- Nice :)

{- 2nd solution: -}

-- IMPLICIT TYPE SIGNATURE: qd4HERE2 :: ExprC r => QDefinition (r String)

qd4HERE2 :: SimpleQDef String -- This could be an alternative to the type above, or they could be used in conjunction.
-- but the problem with 'just' this solution is that we had to manually write the type signature.
qd4HERE2 = QD "qd4'" "shrtname" $ concat (str "q") (str "d4")


{-

Final thought on 2nd variant:

Having a combination of both solutions is harmless too. I don't see why it wouldn't be the
best of both worlds? It would be harmless because users leaving type signatures as "QDefinition (Expr String)" instead
of "SimpleQDef String" wouldn't cause any issues.

-}

{- IM2' variants -}

im12' :: InstanceModel
im12' = IM2' "im1" qd1

im42'' :: InstanceModel
im42'' = IM2' "im4" qd4HERE -- Note that we still need to have the Expr-bound or ModelExpr-bound ones here.

{-
Asides from the extra type tag required for the second version, what is the difference?

Well, when we try to implement something for the IM, we end up with:

```
instance HasShortName InstanceModel where
    shrtName (IM s cmk) = _ -- Uh oh! No guarantee that this will exist!
    shrtName (IM2 s (InstantiableModelKinds2 e)) = _ -- UGLY!
    shrtName (IM2' s e) = shrtName e
```

Well, just implementing "HasShortName" which grabs it's short name from whatever container is internally held appears
to be very difficult.

It looks like the "InstantiableModelKinds2" constructor is fruitless!

It looks like the IM2' variant is the best one.

-}
