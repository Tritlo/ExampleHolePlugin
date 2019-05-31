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

import qualified Hoogle as Hoo

plugin :: Plugin
plugin = defaultPlugin { holeFitPlugin = hfp, pluginRecompile = purePlugin }

hfp :: [CommandLineOption] -> Maybe HoleFitPluginR
hfp opts = Just (HoleFitPluginR init plugin stop)
  where init = intPlugin opts
          --liftIO (putStrLn "initializing") >> newTcRef (coerce (0 :: Int))
        stop = const (liftIO (putStrLn "stopping"))
        plugin ref = HoleFitPlugin (candP opts ref) (fp opts ref)


data HolePluginState = HPS { hpHoogle :: String -> [Hoo.Target] }

initPlugin :: [CommandLineOption] -> TcM (TcRef HolePluginState)
initPlugin li = do
  dbLoc <- case li of
             [hoogleDb] -> return hoogleDb
             _ -> liftIO $ Hoo.defaultDatabaseLocation

  liftIO (putStrLn $ "Using Hoogle database: " <> hoogleDb)
  dbSearch <- liftIO $ Hoo.withDatabase hoogleDb (pure . Hoo.searchDatabase)
  newTcRef (HPS {hpHoogle = dbSearch })


toHoleFitCommand :: TypedHole -> String -> Maybe String
toHoleFitCommand (TyH{holeCt = Just (CHoleCan _ h)}) str
    = stripPrefix ("_" <> str) $ occNameString $ holeOcc h
toHoleFitCommand _ _ = Nothing

holeName :: TypedHole -> Maybe String
holeName (TyH{holeCt = Just (CHoleCan _ h)})
    = Just (occNameString $ holeOcc h)
holeName _ = Nothing



-- | This candidate plugin filters the candidates by module,
--   using the name of the hole as module to search in
candP :: [CommandLineOption] -> TcRef HolePluginState -> CandPlugin
candP _ _ hole cands =
  liftIO (putStrLn $ "candP invoked on " <> (show $ holeName hole)) >>
  case toHoleFitCommand hole "only_" of
    Just modName -> return $ filter (inScopeVia modName) cands
    _ -> return cands
   where inScopeVia modNameStr (GreHFCand gre) =
           elem (toModName modNameStr) $
             map (moduleNameString . importSpecModule) $ gre_imp gre
         inScopeVia _ _ = False
         toModName = replace '_' '.'

replace :: Eq a => a -> a -> [a] -> [a]
replace _ _ [] = []
replace a b (x:xs) = (if x == a then b else x):replace a b xs


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


invokeHoogle :: TcRef HolePluginState -> FitPlugin
invokeHoogle (HPS {hpHoogle = hoogl}) (TyH{holeCt = Just hole}) hfs = do
 flags <- getDynFlags
 let holeTyString :: Ct -> String
     holeTyString = showSDoc dflags . ppr . ctPred
     dispResult :: Hoo.Target -> HoleFit
     dispResult = RawHoleFit . text . Hoo.targetResultDisplay False
     results = map  dispResult $ hoogle $ holeTyString hole
 return (take 3 results)
invokeHoogle _ _ hfs = const hfs


invokeDjinn :: TcRef HolePluginState -> FitPlugin
invokeDjinn _ _ hfs =
  liftIO (putStrLn "djinn integration not implemented yet") >> return hfs


fp :: [CommandLineOption] -> TcRef HolePluginState -> FitPlugin
fp _ rf hole hfs =
  do { case toHoleFitCommand hole "invoke_" of
         Just ('h':'o':'o':'g':'l':'e':rest) -> invokeHoogle rf hole hfs
         Just ('d':'j':'i':'n':'n':rest) -> invokeDjinn rf hole hfs
         _ -> return hfs }
fp _ _ _ hfs = return hfs
