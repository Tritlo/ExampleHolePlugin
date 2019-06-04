{-# LANGUAGE TypeApplications, RecordWildCards #-}
module HolePlugin where

import GhcPlugins hiding ((<>))

import TcHoleErrors

import Data.List (stripPrefix, sortOn)

import TcRnTypes

import TcRnMonad

import Data.Time (UTCTime, NominalDiffTime)
import qualified Data.Time as Time

import Text.Read


data HolePluginState = HPS { timeAlloted :: Maybe NominalDiffTime
                           , elapsedTime :: NominalDiffTime
                           , timeCurStarted :: UTCTime }

bumpElapsed :: NominalDiffTime -> HolePluginState -> HolePluginState
bumpElapsed ad (HPS a e t) = HPS a (e + ad) t

setAlloted :: Maybe NominalDiffTime -> HolePluginState -> HolePluginState
setAlloted a (HPS _ e t) = HPS a e t

setCurStarted :: UTCTime -> HolePluginState -> HolePluginState
setCurStarted nt (HPS a e _) = HPS a e nt

hpStartState :: HolePluginState
hpStartState = HPS Nothing zero undefined
  where zero = fromInteger @NominalDiffTime 0

initPlugin :: [CommandLineOption] -> TcM (TcRef HolePluginState)
initPlugin [msecs] = newTcRef $ hpStartState { timeAlloted = alloted }
  where
    errMsg = "Invalid amount of milliseconds given to plugin: " <> show msecs
    alloted = case readMaybe @Integer msecs of
      Just millisecs -> Just $ fromInteger @NominalDiffTime millisecs / 1000
      _ -> error errMsg
initPlugin _ = newTcRef hpStartState

fromModule :: HoleFitCandidate -> [String]
fromModule (GreHFCand gre) =
  map (moduleNameString . importSpecModule) $ gre_imp gre
fromModule _ = []

toHoleFitCommand :: TypedHole -> String -> Maybe String
toHoleFitCommand TyH{holeCt = Just (CHoleCan _ h)} str
    = stripPrefix ("_" <> str) $ occNameString $ holeOcc h
toHoleFitCommand _ _ = Nothing

-- | This candidate plugin filters the candidates by module,
-- using the name of the hole as module to search in
modFilterTimeoutP :: [CommandLineOption] -> TcRef HolePluginState -> CandPlugin
modFilterTimeoutP _ ref hole cands = do
  curTime <- liftIO Time.getCurrentTime
  HPS {..} <- readTcRef ref
  updTcRef ref (setCurStarted curTime)
  return $ case timeAlloted of
    -- If we're out of time, remove any candidates, so nothing is checked.
    Just sofar | elapsedTime > sofar -> []
    _ -> case toHoleFitCommand hole "only_" of
          Just modName -> filter (inScopeVia modName) cands
          _ -> cands
  where inScopeVia modNameStr cand@(GreHFCand _) =
          elem (toModName modNameStr) $ fromModule cand
        inScopeVia _ _ = False
        toModName = replace '_' '.'
        replace :: Eq a => a -> a -> [a] -> [a]
        replace _ _ [] = []
        replace a b (x:xs) = (if x == a then b else x):replace a b xs

modSortP :: [CommandLineOption] -> TcRef HolePluginState -> FitPlugin
modSortP _ ref hole hfs = do
  curTime <- liftIO Time.getCurrentTime
  HPS {..} <- readTcRef ref
  updTcRef ref $ bumpElapsed (Time.diffUTCTime curTime timeCurStarted)
  return $ case timeAlloted of
    -- If we're out of time, remove any candidates, so nothing is checked.
    Just sofar | elapsedTime > sofar -> [RawHoleFit msg]
    _ -> case toHoleFitCommand hole "sort_by_mod" of
            -- If only_ is on, the fits will all be from the same module.
            Just ('_':'d':'e':'s':'c':_) -> reverse hfs
            Just _ -> orderByModule hfs
            _ ->  hfs
  where orderByModule :: [HoleFit] -> [HoleFit]
        orderByModule = sortOn (fmap fromModule . mbHFCand)
        mbHFCand :: HoleFit -> Maybe HoleFitCandidate
        mbHFCand HoleFit {hfCand = c} = Just c
        mbHFCand _ = Nothing
        msg = hang (text "Error: The time ran out, and the search was aborted for this hole.")
               7 $ text "Try again with a longer timeout."

plugin :: Plugin
plugin = defaultPlugin { holeFitPlugin = holeFitP, pluginRecompile = purePlugin}

holeFitP :: [CommandLineOption] -> Maybe HoleFitPluginR
holeFitP opts = Just (HoleFitPluginR initP pluginDef stopP)
  where initP = initPlugin opts
        stopP = const $ return ()
        pluginDef ref = HoleFitPlugin { candPlugin = modFilterTimeoutP opts ref
                                      , fitPlugin  = modSortP opts ref }
