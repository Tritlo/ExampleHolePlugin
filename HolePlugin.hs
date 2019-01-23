{-# LANGUAGE RecordWildCards #-}
module HolePlugin where

import GhcPlugins

import TcHoleErrors

import Data.List (intersect, stripPrefix)
import RdrName (importSpecModule)

import TcRnTypes

import System.Process

import Data.Maybe (mapMaybe)

import TcRnMonad

import Json

import GenProg

plugin :: Plugin
plugin = defaultPlugin { holeFitPlugin = hfp, pluginRecompile = purePlugin }

hfp :: [CommandLineOption] -> Maybe HoleFitPlugin
hfp opts = Just (HoleFitPlugin (candP opts) (fp opts))


toHoleFitCommand :: TypedHole -> Maybe String
toHoleFitCommand (TyH{holeCt = Just (CHoleCan _ h)})
    = stripPrefix "_with_" (occNameString $ holeOcc h)
toHoleFitCommand _ = Nothing

holeName :: TypedHole -> Maybe String
holeName (TyH{holeCt = Just (CHoleCan _ h)})
    = Just (occNameString $ holeOcc h)
holeName _ = Nothing

-- | This candidate plugin filters the candidates by module,
--   using the name of the hole as module to search in
candP :: [CommandLineOption] -> CandPlugin
candP _ hole cands = do
     case (toHoleFitCommand hole) of
        _ -> return cands

hfName :: HoleFit -> Maybe Name
hfName hf@(HoleFit {}) = (Just . hfCandName . hfCand) hf
hfName _ = Nothing

hfCandName  :: HoleFitCandidate -> Name
hfCandName (IdHFCand id) = idName id
hfCandName (NameHFCand name) = name
hfCandName (GreHFCand gre) = gre_name gre
hfCandName (RawHFCand n _ _) = n


data PropFilterOut = PFO { hName :: Maybe String,
                           pName :: String,
                           hLoc  :: Maybe String,
                           hFits :: [String]} deriving (Show)


fromMaybeNull :: Maybe String -> JsonDoc
fromMaybeNull (Just s) = JSString s
fromMaybeNull _ = JSNull

instance ToJson PropFilterOut where
  json (PFO {..}) = JSObject [ ("file", fromMaybeNull hLoc),
                               ("hole", fromMaybeNull hName),
                               ("prop", JSString pName),
                               ("fits", JSArray $ map JSString hFits)]


hFile :: TypedHole -> Maybe String
hFile (TyH { holeCt = Just (CHoleCan ev _)}) =
   Just (unpackFS (srcSpanFile $ ctLocSpan (ctev_loc ev )))
hFile _ = Nothing

propFilterFP :: String -> String -> FitPlugin
propFilterFP fn name hole fits =
  do fs <- getDynFlags
     mod <- (moduleNameString . moduleName . tcg_mod) <$> getGblEnv
     liftIO $ do putStrLn ("prop was: " ++ name)
                 let fstrings = map (showSDoc fs . ppr) $ (mapMaybe hfName fits)
                     pn = ("prop_" ++ name)
                     pfo = PFO { hName = holeName hole, pName = pn,
                                 hLoc = hFile hole, hFits = fstrings}
                 putStrLn (genProg mod pn fstrings)
                 appendFile fn $ ((showSDoc fs . renderJSON) $ json pfo) ++ ",\n"
                 writeFile (fn ++ ".hs") $ (genProg mod pn fstrings)
                 return fits
 
fp :: [CommandLineOption] -> FitPlugin
fp [fn] hole hfs = case toHoleFitCommand hole of
                  Just name | Just propName <- stripPrefix "prop_" name ->
                      propFilterFP fn propName hole hfs
                  _ -> return hfs
fp _ _ hfs = return hfs
