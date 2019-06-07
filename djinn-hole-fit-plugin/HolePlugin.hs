{-# LANGUAGE TypeApplications, RecordWildCards #-}
module HolePlugin where

import GhcPlugins hiding ((<>))

import TcHoleErrors

import TcRnTypes

import TcRnMonad

import DjinnBridge

import ConLike(conLikeWrapId_maybe)
import TcEnv (tcLookup)
import Data.Maybe (catMaybes)

import Data.List (sortOn)

import qualified Data.Set as Set


data HolePluginState = HPS { djinnEnv :: Environment
                           , maxSols :: MaxSolutions
                           , microSecs :: Int}

setDjinnEnv :: Environment -> HolePluginState -> HolePluginState
setDjinnEnv e (HPS _ sols secs)  = HPS e sols secs

initPlugin :: [CommandLineOption] -> TcM (TcRef HolePluginState)
-- We take more than we need since djinn is prone to duplicate solutions...
initPlugin [sols] = newTcRef $ HPS [] (Max (read @Int sols * 10)) (1000 :: Int)
initPlugin _ = newTcRef $ HPS [] (Max 30) (500 :: Int)


-- | Adds the current candidates to scope in djinn.
djinnAddToScopeP :: [CommandLineOption] -> TcRef HolePluginState -> CandPlugin
djinnAddToScopeP _ ref _ cands = do
  newEnv <- catMaybes <$> mapM hfLookup cands
  --liftIO $ print $ map (showSDocUnsafe . ppr) newEnv
  updTcRef ref (setDjinnEnv newEnv)
  return cands
  where hfLookup :: HoleFitCandidate -> TcM (Maybe (Name,Type))
        hfLookup hfc = tryTcDiscardingErrs (return Nothing) $ do
          let name = getName hfc
          thing <- tcLookup name
          let thingId = case thing of
                          ATcId {tct_id = i} ->  Just i
                          AGlobal (AnId i)   ->  Just i
                          AGlobal (AConLike con) -> conLikeWrapId_maybe con
                          _ -> Nothing
          case thingId of
            Just i -> return $ Just (name, idType i)
            _ -> return Nothing


djinnSynthP :: [CommandLineOption] -> TcRef HolePluginState -> FitPlugin
djinnSynthP _ ref TyH{tyHImplics = imps, tyHCt = Just holeCt} fits = do
  HPS {..} <- readTcRef ref
  let wrappedType = foldl wrapTypeWithImplication (ctPred holeCt) imps
  --liftIO $ print $ map (showSDocUnsafe . ppr) djinnEnv
  -- liftIO $ print (showSDocUnsafe . ppr $ wrappedType)
  let splitSols = unwords . words . unwords . lines
      solToHf = RawHoleFit . parens . text
      Max numToShow = maxSols
  sols <- map splitSols <$> djinn True djinnEnv wrappedType maxSols microSecs
          -- We could set '-fdefer-typed-holes'  and load the module here...
          -- modInfo <- moduleInfo <$>
  let djinnSols = map solToHf $
                    take (numToShow `div` 10) $
                      sortOn length $ dedup sols
  return $ djinnSols <> fits
djinnSynthP _ _ _ _ = return []

-- Lazily de-duplicate a list
dedup :: Ord a => [a] -> [a]
dedup = dedup' Set.empty
  where dedup' sofar (x:xs) | x `Set.member` sofar = dedup' sofar xs
        dedup' sofar (x:xs) = x:dedup' (x `Set.insert` sofar) xs
        dedup' _ [] = []



plugin :: Plugin
plugin = defaultPlugin { holeFitPlugin = holeFitP, pluginRecompile = purePlugin}

holeFitP :: [CommandLineOption] -> Maybe HoleFitPluginR
holeFitP opts = Just (HoleFitPluginR initP pluginDef stopP)
  where initP = initPlugin opts
        stopP = const $ return ()
        pluginDef ref = HoleFitPlugin { candPlugin = djinnAddToScopeP opts ref
                                      , fitPlugin  = djinnSynthP opts ref }
