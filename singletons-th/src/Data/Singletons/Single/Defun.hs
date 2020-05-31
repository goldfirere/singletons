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

import Control.Monad
import Data.Foldable
import Data.Singletons.Names
import Data.Singletons.Promote.Defun
import Data.Singletons.Single.Monad
import Data.Singletons.Single.Type
import Data.Singletons.TH.Options
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
    _  -> do opts     <- getOptions
             sty_ctxt <- mapM singPred ty_ctxt
             names    <- replicateM (length mb_ty_args) $ qNewName "d"
             let tvbs       = zipWith inferMaybeKindTV names mb_ty_args
                 (_, insts) = go opts 0 sty_ctxt [] tvbs
             pure insts
  where
    num_ty_args :: Int
    num_ty_args = length mb_ty_args

    -- The inner loop. @go n ctxt arg_tvbs res_tvbs@ returns @(m_result, insts)@.
    -- Using one particular example:
    --
    -- @
    -- instance (SingI a, SingI b, SEq c, SEq d) =>
    --   SingI (ExampleSym2 (x :: a) (y :: b) :: c ~> d ~> Type) where ...
    -- @
    --
    -- We have:
    --
    -- * @n@ is 2. This is incremented in each iteration of `go`.
    --
    -- * @ctxt@ is (SEq c, SEq d). The (SingI a, SingI b) part of the instance
    --   context is added separately.
    --
    -- * @arg_tvbs@ is [(x :: a), (y :: b)].
    --
    -- * @res_tvbs@ is [(z :: c), (w :: d)]. The kinds of these type variable
    --   binders appear in the result kind.
    --
    -- * @m_result@ is `Just (c ~> d ~> Type)`. @m_result@ is returned so
    --   that earlier defunctionalization symbols can build on the result
    --   kinds of later symbols. For instance, ExampleSym1 would get the
    --   result kind `b ~> c ~> d ~> Type` by prepending `b` to ExampleSym2's
    --   result kind `c ~> d ~> Type`.
    --
    -- * @insts@ are all of the instance declarations corresponding to
    --   ExampleSym2 and later defunctionalization symbols. This is the main
    --   payload of the function.
    --
    -- This function is quadratic because it appends a variable at the end of
    -- the @arg_tvbs@ list at each iteration. In practice, this is unlikely
    -- to be a performance bottleneck since the number of arguments rarely
    -- gets to be that large.
    go :: Options -> Int -> DCxt -> [DTyVarBndr] -> [DTyVarBndr]
       -> (Maybe DKind, [DDec])
    go _    _       _        _        []                 = (mb_ty_res, [])
    go opts sym_num sty_ctxt arg_tvbs (res_tvb:res_tvbs) =
      (mb_new_res, new_inst:insts)
      where
        mb_res :: Maybe DKind
        insts  :: [DDec]
        (mb_res, insts) = go opts (sym_num + 1) sty_ctxt (arg_tvbs ++ [res_tvb]) res_tvbs

        mb_new_res :: Maybe DKind
        mb_new_res = mk_inst_kind res_tvb mb_res

        sing_fun_num :: Int
        sing_fun_num = num_ty_args - sym_num

        mk_sing_fun_expr :: DExp -> DExp
        mk_sing_fun_expr sing_expr =
          foldl' (\f tvb_n -> f `DAppE` (DVarE singMethName `DAppTypeE` DVarT tvb_n))
                 sing_expr
                 (map extractTvbName arg_tvbs)

        singI_ctxt :: DCxt
        singI_ctxt = map (DAppT (DConT singIName) . tvbToType) arg_tvbs

        mk_inst_ty :: DType -> DType
        mk_inst_ty inst_head
          = case mb_new_res of
              Just inst_kind -> inst_head `DSigT` inst_kind
              Nothing        -> inst_head

        arg_tvb_tys :: [DType]
        arg_tvb_tys = map dTyVarBndrToDType arg_tvbs

        -- Construct the arrow kind used to annotate the defunctionalization
        -- symbol (e.g., the `a ~> a ~> Bool` in
        -- `SingI (FooSym0 :: a ~> a ~> Bool)`).
        -- If any of the argument kinds or result kind isn't known (i.e., is
        -- Nothing), then we opt not to construct this arrow kind altogether.
        -- See Note [singDefuns and type inference]
        mk_inst_kind :: DTyVarBndr -> Maybe DKind -> Maybe DKind
        mk_inst_kind tvb' = buildTyFunArrow_maybe (extractTvbKind tvb')

        new_inst :: DDec
        new_inst = DInstanceD Nothing Nothing
                              (sty_ctxt ++ singI_ctxt)
                              (DConT singIName `DAppT` mk_inst_ty defun_inst_ty)
                              [DLetDec $ DValD (DVarP singMethName)
                                       $ wrapSingFun sing_fun_num defun_inst_ty
                                       $ mk_sing_fun_expr sing_exp ]
          where
            defun_inst_ty :: DType
            defun_inst_ty = foldType (DConT (defunctionalizedName opts n sym_num))
                                     arg_tvb_tys

            sing_exp :: DExp
            sing_exp = case ns of
                         DataName -> DConE $ singledDataConName opts n
                         _        -> DVarE $ singledValueName opts n

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
singletons-th.
-}
