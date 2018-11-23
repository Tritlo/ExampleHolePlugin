{-# OPTIONS -dynamic #-}
module HolePlugin where

import GhcPlugins

import TcHoleErrors

import Data.List (intersect, stripPrefix)
import RdrName (importSpecModule)

import TcRnTypes

plugin :: Plugin
plugin = defaultPlugin { holeFitPlugin = hfp, pluginRecompile = purePlugin }

hfp :: [CommandLineOption] -> Maybe HoleFitPlugin
hfp modname = Just (HoleFitPlugin (candP modname) fp)


propName :: Maybe String -> Maybe String
propName = flip (>>=) (stripPrefix "_satisfies_")

candOccNameString :: HoleFitCandidate -> String
candOccNameString (IdHFCand hfid) = occNameString $ occName hfid
candOccNameString (GreHFCand gre) = occNameString $ occName $ gre_name gre
candOccNameString (NameHFCand name) = occNameString $ occName name

findProp :: String -> [HoleFitCandidate] -> [HoleFitCandidate]
findProp str to_check = findProp' [] to_check
  where
    findProp' sofar (cand:cands) =
      do if (candOccNameString cand) == str
          then keep_it else discard
      where discard = findProp' sofar cands
            keep_it = findProp' (cand:sofar) cands
    findProp' sofar _ = reverse sofar

candP :: [CommandLineOption] -> CandPlugin
candP opts hole cands =
  do let he = case holeCt hole of
                Just (CHoleCan _ h) -> Just (occNameString $ holeOcc h)
                _ -> Nothing
     case propName he of
       Just p -> do liftIO $ print p
                    liftIO $ print $ map candOccNameString $ findProp p cands
       _ -> return ()
     return $ filter greNotInOpts cands
  where greNotInOpts (GreHFCand gre) = not $ null $ intersect (inScopeVia gre) opts
        greNotInOpts _ = True
        inScopeVia = map (moduleNameString . importSpecModule) . gre_imp

fp :: FitPlugin
fp _ hfs = return hfs
