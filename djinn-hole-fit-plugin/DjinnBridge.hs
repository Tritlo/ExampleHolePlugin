--- Based on djinn-ghc by Alejandro Serrano, but reworked to use TcM directly.

{-# LANGUAGE CPP, PatternGuards, BangPatterns #-}
module DjinnBridge (Environment, MaxSolutions(..), djinn) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad (forM)
import Data.Set (Set, insert, union, unions, empty, toList)

import qualified Djinn.HTypes as D
import qualified Djinn.LJT as D

import qualified TcEnv as TE
import TcRnTypes
import qualified GhcPlugins as GP

import MonadUtils
import qualified DataCon as G
import qualified GHC as G
import qualified Name as G
import qualified TyCon as G
import qualified Type as G

import Data.Maybe (mapMaybe, catMaybes, isJust)


data NoExtraInfo = NoExtraInfo
type HEnvironment1 a = [(D.HSymbol, ([D.HSymbol], D.HType, a))]
type HEnvironment = HEnvironment1 NoExtraInfo

getConTs :: G.Type -> Set G.Name
getConTs t | Just (_, i)  <- G.splitForAllTy_maybe t = getConTs i
getConTs t | Just (t1,t2) <- G.splitFunTy_maybe t    = getConTs t1 `union` getConTs t2
getConTs t | Just (c, ts) <- G.splitTyConApp_maybe t =
  let args = unions $ map getConTs ts
   in if G.isTupleTyCon c then args else insert (G.getName c) args
getConTs t | Just (t1,t2) <- G.splitAppTy_maybe t    = getConTs t1 `union` getConTs t2
getConTs t | Just _       <- G.getTyVar_maybe t      = empty
getConTs _                                           = empty

mbHType :: G.Type -> Maybe D.HType
mbHType t | Just (_, i)  <- G.splitForAllTy_maybe t = mbHType  i
mbHType t | Just (t1,t2) <- G.splitFunTy_maybe t    = do ht1 <- mbHType t1
                                                         ht2 <- mbHType t2
                                                         return $ D.HTArrow ht1 ht2
mbHType t | Just (c, ts) <- G.splitTyConApp_maybe t = do
              args <- mapM mbHType ts
              if G.isTupleTyCon c  -- Check if we have a tuple
                then if not (null args)
                     then Just $ D.HTTuple args
                     -- The unit constructor () is also a tupeTyCon, but this case
                     -- causes the show instance in Djinn to fail, so we drop it.
                     else Nothing
                else Just $ createHTApp (G.getOccString c) (reverse args)
  where createHTApp n []     = D.HTCon n
        createHTApp n (x:xs) = D.HTApp (createHTApp n xs) x
mbHType t | Just (t1,t2) <- G.splitAppTy_maybe t  = do ht1 <- mbHType t1
                                                       ht2 <- mbHType t2
                                                       return $ D.HTApp ht1 ht2
mbHType t | Just var <- G.getTyVar_maybe t          = Just $ D.HTVar (toHSymbol var)
mbHType _                                           = Nothing

environment :: G.Type -> TcM HEnvironment
environment ty = do
  let tyConTs = getConTs ty
  concat <$> mapM environment1 (toList tyConTs)

environment1 :: G.Name -> TcM HEnvironment
environment1 name = do
  thing <- TE.tcLookupGlobal name
  case thing of
    G.ATyCon tycon | G.isAlgTyCon tycon -> do
      let tyconName = toHSymbol $ G.tyConName tycon
          varsH = map toHSymbol $ G.tyConTyVars tycon
          Just datacons = G.tyConDataCons_maybe tycon
      dtypes <- forM datacons $ \dcon -> do
        let dconN = toHSymbol $ G.dataConName dcon
            (_,_,dconT,_) = G.dataConSig dcon
        dconE <- mapM environment dconT
        return $ do dconTTys <- mapM mbHType dconT
                    return ((dconN, dconTTys), dconE)
      return $ if all isJust dtypes
               then let dtypesT = map fst $ catMaybes dtypes
                        dtypesE = concatMap snd $ catMaybes dtypes
                    in (tyconName, (varsH, D.HTUnion dtypesT, NoExtraInfo)) : concat dtypesE
               else []
    G.ATyCon tycon | G.isTypeSynonymTyCon tycon -> do
      -- Get information for this type synonym
      let tyconName = toHSymbol $ G.tyConName tycon
          Just (vars, defn) = G.synTyConDefn_maybe tycon
          varsH = map toHSymbol vars
      -- Recursively obtain it for the environment of the type
      case mbHType defn of
        Just htype -> do defnEnv <- environment defn
                         return $ (tyconName, (varsH, htype, NoExtraInfo)) : defnEnv
        _ -> return []
    _ -> return []
    --         return []

toHSymbol :: G.NamedThing a => a -> D.HSymbol
toHSymbol = G.getOccString

toLJTSymbol :: G.NamedThing a => a -> D.Symbol
toLJTSymbol = D.Symbol . G.getOccString

-- |Bindings which are in scope at a specific point.
type Environment = [(G.Name, G.Type)]

-- |Obtain a maximum number of solutions.
newtype MaxSolutions = Max Int


-- |Obtain the list of expressions which could fill
-- something with the given type.
-- The first flag specifies whether to return one
-- or more solutions to the problem.
djinn :: Bool -> Environment -> G.Type -> MaxSolutions -> Int -> TcM [String]
djinn multi env ty (Max mx) microsec = do
  tyEnv <- environment ty
  case mbHType ty of
    Just hT -> let form = D.hTypeToFormula tyEnv hT
                   toEnvF (n, t) =
                      case mbHType t of
                        Just ht -> Just (toLJTSymbol n, D.hTypeToFormula tyEnv ht)
                        _ -> Nothing
                   envF = mapMaybe toEnvF env
                   prfs = D.prove multi envF form
                   trms =  map (D.hPrExpr . D.termToHExpr ) prfs
               in liftIO $ cropList trms microsec mx (\x -> GP.lengthLessThan x 1000)
    _ -> return []

cropList :: [a] -> Int -> Int -> (a -> Bool) -> IO [a]
cropList _   _  0 _ = return []
cropList lst ms n chk =
  withAsync (let !l = lst in return l) $ \a -> do
    threadDelay ms
    res <- poll a
    case res of
      Just r -> case r of
        Right (x:xs) -> if chk x then do ys <- cropList xs ms (n-1) chk
                                         return $ x : ys
                                 else return []
        _            -> return []
      Nothing -> do cancel a
                    return []
