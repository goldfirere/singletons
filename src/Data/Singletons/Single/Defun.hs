-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Singletons.Single.Defun
-- Copyright   :  (C) 2018 Ryan Scott
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Ryan Scott
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Creates 'SingI' instances for promoted types' defunctionalization symbols.
--
-----------------------------------------------------------------------------

module Data.Singletons.Single.Defun (singDefuns) where

import Data.List
import Data.Singletons.Names
import Data.Singletons.Promote.Defun
import Data.Singletons.Single.Monad
import Data.Singletons.Single.Type
import Data.Singletons.Util
import Language.Haskell.TH.Desugar
import Language.Haskell.TH.Syntax

-- Given the Name of something, take the defunctionalization symbols for its
-- promoted counterpart and create SingI instances for them. As a concrete
-- example, if you have:
--
--   foo :: Eq a => a -> a -> Bool
--
-- Then foo's promoted counterpart, Foo, will have two defunctionalization
-- symbols:
--
--   FooSym0 :: a ~> a ~> Bool
--   FooSym1 :: a -> a ~> Bool
--
-- We can declare SingI instances for these two symbols like so:
--
--   instance SEq a => SingI (FooSym0 :: a ~> a ~> Bool) where
--     sing = singFun2 sFoo
--
--   instance (SEq a, SingI x) => SingI (FooSym1 x :: a ~> Bool) where
--     sing = singFun1 (sFoo (sing @_ @x))
--
-- Note that singDefuns takes Maybe DKinds for the promoted argument and result
-- types, in case we have an entity whose type needs to be inferred.
-- See Note [singDefuns and type inference].
singDefuns :: Name      -- The Name of the thing to promote.
           -> NameSpace -- Whether the above Name is a value, data constructor,
                        -- or a type constructor.
           -> DCxt      -- The type's context.
           -> [Maybe DKind] -- The promoted argument types (if known).
           -> Maybe DKind   -- The promoted result type (if known).
           -> SgM [DDec]
singDefuns n ns ty_ctxt mb_ty_args mb_ty_res =
  case mb_ty_args of
    [] -> pure [] -- If a function has no arguments, then it has no
                  -- defunctionalization symbols, so there's nothing to be done.
    _  -> do sty_ctxt <- mapM singPred ty_ctxt
             go 0 sty_ctxt [] mb_ty_args
  where
    num_ty_args :: Int
    num_ty_args = length mb_ty_args

    -- Sadly, this algorithm is quadratic, because in each iteration of the loop
    -- we must:
    --
    -- * Construct an arrow type of the form (a ~> ... ~> z), using a suffix of
    --   the promoted argument types.
    -- * Append a new type variable to the end of an ordered list.
    --
    -- In practice, this is unlikely to be a bottleneck, as singletons does not
    -- support functions with more than 7 or so arguments anyways.
    go :: Int -> DCxt -> [DTyVarBndr] -> [Maybe DKind] -> SgM [DDec]
    go sym_num sty_ctxt tvbs mb_tyss
      | sym_num < num_ty_args
      , mb_ty:mb_tys <- mb_tyss
      = do new_tvb_name <- qNewName "d"
           let new_tvb = inferMaybeKindTV new_tvb_name mb_ty
           insts <- go (sym_num + 1) sty_ctxt (tvbs ++ [new_tvb]) mb_tys
           pure $ new_insts ++ insts
      | otherwise
      = pure []
      where
        sing_fun_num :: Int
        sing_fun_num = num_ty_args - sym_num

        mk_sing_fun_expr :: DExp -> DExp
        mk_sing_fun_expr sing_expr =
          foldl' (\f tvb_n -> f `DAppE` (DVarE singMethName `DAppTypeE` DVarT tvb_n))
                 sing_expr
                 (map extractTvbName tvbs)

        singI_ctxt :: DCxt
        singI_ctxt = map (DAppT (DConT singIName) . tvbToType) tvbs

        mk_inst_ty :: DType -> DType
        mk_inst_ty inst_head
          = case mb_inst_kind of
              Just inst_kind -> inst_head `DSigT` inst_kind
              Nothing        -> inst_head

        tvb_tys :: [DType]
        tvb_tys = map dTyVarBndrToDType tvbs

        -- Construct the arrow kind used to annotate the defunctionalization
        -- symbol (e.g., the `a ~> a ~> Bool` in
        -- `SingI (FooSym0 :: a ~> a ~> Bool)`).
        -- If any of the argument kinds or result kind isn't known (i.e., is
        -- Nothing), then we opt not to construct this arrow kind altogether.
        -- See Note [singDefuns and type inference]
        mb_inst_kind :: Maybe DType
        mb_inst_kind = foldr buildTyFunArrow_maybe mb_ty_res mb_tyss

        new_insts :: [DDec]
        new_insts = [DInstanceD Nothing
                                (sty_ctxt ++ singI_ctxt)
                                (DConT singIName `DAppT` mk_inst_ty defun_inst_ty)
                                [DLetDec $ DValD (DVarP singMethName)
                                         $ wrapSingFun sing_fun_num defun_inst_ty
                                         $ mk_sing_fun_expr sing_exp ]]
          where
            defun_inst_ty :: DType
            defun_inst_ty = foldType (DConT (promoteTySym n sym_num)) tvb_tys

            sing_exp :: DExp
            sing_exp = case ns of
                         DataName -> DConE $ singDataConName n
                         _        -> DVarE $ singValName n

{-
Note [singDefuns and type inference]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider the following function:

  foo :: a -> Bool
  foo _ = True

singDefuns would give the following SingI instance for FooSym0, with an
explicit kind signature:

  instance SingI (FooSym0 :: a ~> Bool) where ...

What happens if we leave off the type signature for foo?

  foo _ = True

Can singDefuns still do its job? Yes! It will simply generate:

  instance SingI FooSym0 where ...

In general, if any of the promoted argument or result types given to singDefun
are Nothing, then we avoid crafting an explicit kind signature. You might worry
that this could lead to SingI instances being generated that GHC cannot infer
the type for, such as:

  bar x = x == x
  ==>
  instance SingI BarSym0 -- Missing an SEq constraint?

This is true, but also not unprecedented, as the singled version of bar, sBar,
will /also/ fail to typecheck due to a missing SEq constraint. Therefore, this
design choice fits within the existing tradition of type inference in
singletons.
-}
