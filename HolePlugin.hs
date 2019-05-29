{-# LANGUAGE RecordWildCards #-}
module HolePlugin where

import GhcPlugins hiding ((<>))

import Data.Coerce (coerce)

import TcHoleErrors

import Data.List (intersect, stripPrefix)
import RdrName (importSpecModule)

import TcRnTypes

import System.Process

import Data.Maybe (mapMaybe)

import TcRnMonad

import Json

import Test.ProgInput

import Data.Hashable

import Debug.Trace

plugin :: Plugin
plugin = defaultPlugin { holeFitPlugin = hfp, pluginRecompile = purePlugin }

hfp :: [CommandLineOption] -> Maybe HoleFitPluginR
hfp opts = Just (HoleFitPluginR init plugin stop)
  where init =  liftIO (putStrLn "initializing") >> newTcRef (coerce (0 :: Int))
        stop = const (liftIO (putStrLn "stopping"))
        plugin ref = HoleFitPlugin (candP opts ref) (fp opts ref)


toHoleFitCommand :: TypedHole -> String -> Maybe String
toHoleFitCommand (TyH{holeCt = Just (CHoleCan _ h)}) str
    = stripPrefix ("_" <> str <> "_") $ occNameString $ holeOcc h
toHoleFitCommand _ _ = Nothing

holeName :: TypedHole -> Maybe String
holeName (TyH{holeCt = Just (CHoleCan _ h)})
    = Just (occNameString $ holeOcc h)
holeName _ = Nothing


newtype HolePluginState = HPS Int
  deriving (Show)

bumpHPS :: HolePluginState -> HolePluginState
--bumpHPS (HPS a) = HPS (a + 1)
bumpHPS = coerce ((+ 1) :: Int -> Int)


-- | This candidate plugin filters the candidates by module,
--   using the name of the hole as module to search in
candP :: [CommandLineOption] -> TcRef HolePluginState -> CandPlugin
candP _ rf hole cands =
  liftIO (putStrLn $ "candP invoked on " <> (show $ holeName hole)) >>
  case toHoleFitCommand hole "only" of
                          Just modName -> return $ filter (inScopeVia modName) cands
                          _ -> do {r <- readTcRef rf
                                  ; liftIO $ putStrLn ("non 'only's so far: " ++ show r)
                                  ; updTcRef rf bumpHPS
                                  ; return cands }
   where inScopeVia modNameStr (GreHFCand gre) =
           elem (toModName modNameStr) $
             map (moduleNameString . importSpecModule) $ gre_imp gre
         inScopeVia _ _ = False
         toModName = replace '_' '.'

replace :: Eq a => a -> a -> [a] -> [a]
replace _ _ [] = []
replace a b (x:xs) = (if x == a then b else x):replace a b xs


data PropFilterOut = PFO { hName :: Maybe String,
                           pName :: String,
                           hLoc  :: Maybe String,
                           hFits :: [String]} deriving (Show)

data ShouldFilterOut = SFO { shName :: Maybe String,
                             spName :: String,
                             shLoc  :: Maybe String,
                             shFits :: [String]} deriving (Show)


fromMaybeNull :: Maybe String -> JsonDoc
fromMaybeNull (Just s) = JSString s
fromMaybeNull _ = JSNull


hFile :: TypedHole -> Maybe String
hFile (TyH { holeCt = Just (CHoleCan ev _)}) =
   Just (unpackFS (srcSpanFile $ ctLocSpan (ctev_loc ev )))
hFile _ = Nothing

hfName :: HoleFit -> Maybe String
hfName (RawHoleFit _) = Nothing
hfName hf = Just $ getOccString $ hfCand hf

propFilterFP :: String -> String -> FitPlugin
propFilterFP fn name hole fits =
  do fs <- getDynFlags
     mod <- (moduleNameString . moduleName . tcg_mod) <$> getGblEnv
     liftIO $ do putStrLn ("prop was: " ++ name)
                 let fstrings = map (showSDoc fs . ppr) $ (mapMaybe hfName fits)
                     pn = ("prop_" ++ name)
                     pfo = PFO { hName = holeName hole, pName = pn,
                                 hLoc = hFile hole, hFits = fstrings}
                 appendFile fn $ ( Prelude.<> "\n") $ show $ (ProgIn {modN = mod, propN = pn,
                  fitStrs = fstrings, holeN = holeName hole,
                  holeL = hFile hole})
                 return fits

shouldFilterFP :: String -> String -> FitPlugin
shouldFilterFP fn name hole fits =
  do fs <- getDynFlags
     mod <- (moduleNameString . moduleName . tcg_mod) <$> getGblEnv
     liftIO $ do putStrLn ("should was: " ++ name)
                 let fstrings = map (showSDoc fs . ppr) $ (mapMaybe hfName fits)
                     sfo = SFO { shName = holeName hole, spName = name,
                                 shLoc = hFile hole, shFits = fstrings}
                 appendFile fn $ ( Prelude.<> "\n") $ show $
                    (ProgIn {modN = mod, propN = name,
                             fitStrs = fstrings, holeN = holeName hole,
                             holeL = hFile hole})
                 return fits

fp :: [CommandLineOption] -> TcRef HolePluginState -> FitPlugin
fp [fn] rf hole hfs =
  do { r <- readTcRef rf
     ; liftIO $ putStrLn ("fitP ref was: " ++ show r)
     ; updTcRef rf bumpHPS
     ; case toHoleFitCommand hole "with" of
         Just name | Just propName <- stripPrefix "prop_" name ->
                     propFilterFP fn propName hole hfs
         Just name | Just shouldName <- stripPrefix "should_" name ->
                     shouldFilterFP fn shouldName hole hfs
         _ -> return hfs }
fp _ _ _ hfs = return hfs
