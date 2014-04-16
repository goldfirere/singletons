{- Data/Singletons/Single/Monad.hs

(c) Richard Eisenberg 2014
eir@cis.upenn.edu

This file defines the SgM monad and its operations, for use during singling.

The SgM monad allows reading from a SgEnv environment and is wrapped around a Q.
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Singletons.Single.Monad (
  SgM, bindProxies, bindVars, bindLets, lookupProxy, liftPrM,
  singM, singDecsM,
  ProxyFlag(..), ProxySpec, ProxyTable,
  emitDecs, emitDecsM
  ) where

import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Singletons.Promote.Monad
import Data.Singletons.Names
import Language.Haskell.TH.Syntax hiding ( lift )
import Language.Haskell.TH.Desugar
import Control.Applicative
import Data.Traversable ( traverse )
import Control.Monad.Reader
import Control.Monad.Writer

-- avoid Bool
data ProxyFlag = YesProxy | NoProxy
  deriving (Eq, Show)

type ProxySpec  = [ProxyFlag]

-- ProxyTable stores a mapping between function name and whether or
-- not an extra Proxy parameter is required by this function's arguments. For
-- example if ProxyTable stores mapping "foo -> [No,Yes,No]" it means that
-- foo accepts three parameters and that second parameter is a
-- function that requires one extra Proxy passed as argument when foo
-- is applied in the body of foo.
type ProxyTable = Map Name ProxySpec

-- environment during singling
data SgEnv =
  SgEnv { sg_proxies      :: ProxyTable   -- map from the *singletonized* name
        }

emptySgEnv :: SgEnv
emptySgEnv = SgEnv { sg_proxies      = Map.empty
                   }

-- the singling monad
newtype SgM a = SgM (ReaderT SgEnv PrM a)
  deriving ( Functor, Applicative, Monad, Quasi
           , MonadReader SgEnv, MonadWriter [DDec] )

bindProxies :: ProxyTable -> SgM a -> SgM a
bindProxies proxies1 =
  local (\env@(SgEnv { sg_proxies = proxies2 }) ->
               env { sg_proxies = proxies1 `Map.union` proxies2 })

bindVars :: VarPromotions -> SgM a -> SgM a
bindVars var_proms (SgM thing_inside) = SgM $ do
  sg_env <- ask
  let prm = runReaderT thing_inside sg_env
  lift $ lambdaBind var_proms prm

bindLets :: [LetBind] -> SgM a -> SgM a
bindLets lets (SgM thing_inside) = SgM $ do
  sg_env <- ask
  let prm = runReaderT thing_inside sg_env
  lift $ letBind lets prm

lookupProxy :: Name -> SgM ProxySpec
lookupProxy name = do
  proxyTable <- asks sg_proxies
  case Map.lookup name proxyTable of
    Nothing -> do
      -- try to get it from the global context
      m_info <- qRecover (return Nothing) (fmap Just $ qReify name)
      m_dinfo <- traverse dsInfo m_info
      case m_dinfo of
        Just (DVarI _ ty _ _) -> return $ mk_proxy_spec ty
        _                     -> do
--          qReportWarning $ "Proxy lookup failed for " ++ show name ++ "\n"
--                        ++ show proxyTable
          return $ repeat NoProxy
    Just pr -> return pr

  where
    mk_proxy_spec :: DType -> ProxySpec
    mk_proxy_spec (DForallT _ _ ty) = mk_proxy_spec ty
    mk_proxy_spec (DAppT (DAppT DArrowT (DAppT (DConT myProxyName) _))
                         (DAppT (DAppT DArrowT _) rest))
      | myProxyName == proxyTypeName = YesProxy : mk_proxy_spec rest
    mk_proxy_spec (DAppT (DAppT DArrowT _) rest) = NoProxy : mk_proxy_spec rest
    mk_proxy_spec _ = []

liftPrM :: PrM a -> SgM a
liftPrM prm = SgM $ lift prm  

singM :: Quasi q => SgM a -> q (a, [DDec])
singM (SgM rdr) =
  let prm = runReaderT rdr emptySgEnv in
  promoteM prm

singDecsM :: Quasi q => SgM [DDec] -> q [DDec]
singDecsM thing = do
  (decs1, decs2) <- singM thing
  return $ decs1 ++ decs2
