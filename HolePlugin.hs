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
toFilter = flip (>>=) (stripPrefix "module_")

replace :: Eq a => a -> a -> [a] -> [a]
replace match repl str = replace' [] str
  where
    replace' sofar (x:xs) | x == match = replace' (repl:sofar) xs
    replace' sofar (x:xs) = replace' (x:sofar) xs
    replace' sofar [] = reverse sofar

toHoleFitCommand :: TypedHole -> Maybe String
toHoleFitCommand (TyH{holeCt = Just (CHoleCan _ h)})
    = stripPrefix "_with_" (occNameString $ holeOcc h)
toHoleFitCommand _ = Nothing


hoogleCP :: CandPlugin
hoogleCP _ _ = return []

modFilterCP :: String -> CandPlugin
modFilterCP modName _ cands =
    return $ filter (greNotInOpts [(replace '_' '.' modName)]) cands
  where greNotInOpts opts (GreHFCand gre) =
            not $ null $ intersect (inScopeVia gre) opts
        greNotInOpts _ _ = True
        inScopeVia = map (moduleNameString . importSpecModule) . gre_imp


-- | This candidate plugin filters the candidates by module,
--   using the name of the hole as module to search in
candP :: [CommandLineOption] -> CandPlugin
candP _ hole cands = do
     case (toHoleFitCommand hole) of
        Just "hoogle" -> hoogleCP hole cands
        Just name | Just modName <- stripPrefix "module_" name ->
            modFilterCP modName hole cands
        _ -> return cands

-- Yes, it's pretty hacky, but it is just an example :)
searchHoogle :: String -> IO [String]
searchHoogle ty = lines <$> (readProcess "hoogle" [(show ty)] [])


propFilterFP :: String -> FitPlugin
propFilterFP name hole fits = liftIO $ (putStrLn ("prop was: " ++ name)) >> return fits

fp :: [CommandLineOption] -> FitPlugin
fp _ hole hfs = case toHoleFitCommand hole of
                  Just "hoogle" ->
                    do dflags <- getDynFlags
                       let tyString = showSDoc dflags . ppr . ctPred <$> holeCt hole
                       res <- case tyString of
                                Just ty -> liftIO $ searchHoogle ty
                                _ -> return []
                       return $ (take 2
                              $ map (RawHoleFit [] Nothing . text
                                    . ("Hoogle says: " ++)) res)
                  Just name | Just propName <- stripPrefix "prop_" name ->
                      propFilterFP propName hole hfs
                  _ -> return hfs
