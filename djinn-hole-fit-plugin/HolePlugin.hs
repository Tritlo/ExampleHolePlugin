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



data HolePluginState = HPS { djinnEnv :: Environment
                           , maxSols :: MaxSolutions
                           , microSecs :: Int}

setDjinnEnv :: Environment -> HolePluginState -> HolePluginState
setDjinnEnv e (HPS _ sols secs)  = HPS e sols secs

initPlugin :: [CommandLineOption] -> TcM (TcRef HolePluginState)
-- initPlugin [microsecs] =
--   newTcRef $ HPS [] (Max 15) (read @Int microsecs)
-- initPlugin [microsecs, maxsols] =
--   newTcRef $ HPS [] (Max $ read @Int maxsols) (read @Int microsecs)
initPlugin _ = newTcRef $ HPS [] (Max 6) (100000 :: Int)


-- | Adds the current candidates to scope in djinn.
djinnAddToScopeP :: [CommandLineOption] -> TcRef HolePluginState -> CandPlugin
djinnAddToScopeP _ ref _ cands = do
  newEnv <- catMaybes <$> mapM hfLookup cands
  --liftIO $ print $ map (showSDocUnsafe . ppr) newEnv
  updTcRef ref (setDjinnEnv newEnv)
  return []
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
djinnSynthP _ ref TyH{implics = imps, holeCt = Just holeCt} _ = do
  HPS {..} <- readTcRef ref
  let wrappedType = foldl wrapTypeWithImplication (ctPred holeCt) imps
  --liftIO $ print $ map (showSDocUnsafe . ppr) djinnEnv
  -- liftIO $ print (showSDocUnsafe . ppr $ wrappedType)
  sols <- djinn True djinnEnv wrappedType maxSols microSecs
          -- We could set '-fdefer-typed-holes'  and load the module here...
          -- modInfo <- moduleInfo <$>
  return $ map (RawHoleFit . parens . text .
                            unwords . words . unwords . lines ) sols

djinnSynthP _ _ _ _ = return []

plugin :: Plugin
plugin = defaultPlugin { holeFitPlugin = holeFitP, pluginRecompile = purePlugin}

holeFitP :: [CommandLineOption] -> Maybe HoleFitPluginR
holeFitP opts = Just (HoleFitPluginR initP pluginDef stopP)
  where initP = initPlugin opts
        stopP = const $ return ()
        pluginDef ref = HoleFitPlugin { candPlugin = djinnAddToScopeP opts ref
                                      , fitPlugin  = djinnSynthP opts ref }
