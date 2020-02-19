module HolePlugin where

import GhcPlugins

import TcHoleErrors

import Data.List (intersect, stripPrefix)
import RdrName (importSpecModule)

import Constraint

import System.Process

plugin :: Plugin
plugin = defaultPlugin { holeFitPlugin = hfp, pluginRecompile = purePlugin }

hfp :: [CommandLineOption] -> Maybe HoleFitPluginR
hfp opts = Just (fromPureHFPlugin $ HoleFitPlugin (candP opts) (fp opts))

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
  do let he = case tyHCt hole of
                Just (CHoleCan _ h ExprHole) -> Just (occNameString h)
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
       let tyString = showSDoc dflags . ppr . ctPred <$> tyHCt hole
       res <- case tyString of
                Just ty -> liftIO $ searchHoogle ty
                _ -> return []
       return $ (take 2 $ map (RawHoleFit . text . ("Hoogle says: " ++)) res) ++ hfs
fp _ _ hfs = return hfs
