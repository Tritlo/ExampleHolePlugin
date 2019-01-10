module HolePlugin where

import GhcPlugins

import TcHoleErrors

import Data.List (intersect, stripPrefix)
import RdrName (importSpecModule)

import TcRnTypes

import System.Process

plugin :: Plugin
plugin = defaultPlugin { holeFitPlugin = hfp, pluginRecompile = purePlugin }

hfp :: [CommandLineOption] -> Maybe HoleFitPlugin
hfp opts = Just (HoleFitPlugin (candP opts) (fp opts))

toFilter :: Maybe String -> Maybe String
toFilter = flip (>>=) (stripPrefix "_module_")

replace :: Eq a => a -> a -> [a] -> [a]
replace match repl str = replace' [] str
  where
    replace' sofar (x:xs) | x == match = replace' (repl:sofar) xs
    replace' sofar (x:xs) = replace' (x:sofar) xs
    replace' sofar [] = reverse sofar

-- | This candidate plugin filters the candidates by module,
--   using the name of the hole as module to search in
candP :: [CommandLineOption] -> CandPlugin
candP _ hole cands =
  do let he = case holeCt hole of
                Just (CHoleCan _ h) -> Just (occNameString $ holeOcc h)
                _ -> Nothing
     case toFilter he of
        Just undscModName -> do let replaced = replace '_' '.' undscModName
                                let res = filter (greNotInOpts [replaced]) cands
                                return $ res 
        _ -> return cands
  where greNotInOpts opts (GreHFCand gre)  = not $ null $ intersect (inScopeVia gre) opts
        greNotInOpts _ _ = True
        inScopeVia = map (moduleNameString . importSpecModule) . gre_imp

-- Yes, it's pretty hacky, but it is just an example :)
searchHoogle :: String -> IO [String]
searchHoogle ty = lines <$> (readProcess "hoogle" [(show ty)] [])

fp :: [CommandLineOption] -> FitPlugin
fp ("hoogle":[]) hole hfs =
    do dflags <- getDynFlags
       let tyString = showSDoc dflags . ppr . ctPred <$> holeCt hole
       res <- case tyString of
                Just ty -> liftIO $ searchHoogle ty
                _ -> return []
       return $ (take 2 $ map (RawHoleFit [] Nothing . text .("Hoogle says: " ++)) res) ++ hfs
fp _ _ hfs = return hfs
