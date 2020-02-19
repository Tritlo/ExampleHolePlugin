module HPlusPlugin where

import GhcPlugins

import TcHoleErrors

import Constraint (ctPred)

import System.Process

plugin :: Plugin
plugin = defaultPlugin { holeFitPlugin = hfp, pluginRecompile = purePlugin }

hfp :: [CommandLineOption] -> Maybe HoleFitPluginR
hfp opts = Just (fromPureHFPlugin $ HoleFitPlugin (candP opts) (fp opts))

-- | This candidate plugin filters the candidates by module,
--   using the name of the hole as module to search in
candP :: [CommandLineOption] -> CandPlugin
candP _ _ _ = return []

-- Yes, it's pretty hacky, but it is just an example :)
hPlus :: String -> String -> IO [String]
hPlus hploc ty = lines <$> (readProcess "../lookup" [hploc, ty] [])

fp :: [CommandLineOption] -> FitPlugin
fp [hploc] hole hfs =
    do dflags <- getDynFlags
       let tyString = showSDoc dflags . ppr . ctPred <$> tyHCt hole
       res <- case tyString of
                Just ty -> liftIO $ hPlus hploc ty
                _ -> return []
       return $ (take 5 $ map (RawHoleFit . text ) res) ++ hfs
fp r _ _ = return $ map (RawHoleFit . text) r
