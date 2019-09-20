{-# LANGUAGE TypeApplications, RecordWildCards #-}
module ExtendedHolesPlugin where

import GhcPlugins hiding ((<>))

import TcHoleErrors

import TcRnTypes

import TcRnMonad

import DjinnBridge

import ConLike(conLikeWrapId_maybe)
import TcEnv (tcLookup)
import Data.Maybe (catMaybes)

import Data.List (sortOn)

import qualified Data.Set as Set

import Data.List (intersect, stripPrefix)
-- import RdrName (importSpecModule)

import System.Process

import HsExpr
import HsLit

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH (Q, Exp)
import Data.Char (toLower)

import Debug.Trace


invoke :: String -> Q Exp
invoke str = return $ TH.LitE $ TH.StringL ("invoke " ++ (map toLower str))
filterBy :: String -> Q Exp
filterBy str = return $ TH.LitE $ TH.StringL ("module " ++ str)

(&) :: Q Exp -> Q Exp -> Q Exp
(&) e1 e2 = do TH.LitE (TH.StringL str1) <- e1
               TH.LitE (TH.StringL str2) <- e2
               return $ TH.LitE  $ TH.StringL  $ str1 ++ "&" ++ str2
                  



data HolePluginState = HPS { djinnEnv :: Environment
                           , maxSols :: MaxSolutions
                           , microSecs :: Int}

setDjinnEnv :: Environment -> HolePluginState -> HolePluginState
setDjinnEnv e (HPS _ sols secs)  = HPS e sols secs

initPlugin :: [CommandLineOption] -> TcM (TcRef HolePluginState)
-- We take more than we need since djinn is prone to duplicate solutions...
initPlugin _ = newTcRef $ HPS [] (Max 20) (40000 :: Int)





-- | Adds the current candidates to scope in djinn.
djinnAddToScopeP :: TcRef HolePluginState -> CandPlugin
djinnAddToScopeP ref _ cands = do
  newEnv <- catMaybes <$> mapM hfLookup cands
  --liftIO $ print $ map (showSDocUnsafe . ppr) newEnv
  updTcRef ref (setDjinnEnv newEnv)
  return cands
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


djinnSynthP :: TcRef HolePluginState -> FitPlugin
djinnSynthP ref TyH{tyHImplics = imps, tyHCt = Just holeCt} fits = do
  HPS {..} <- readTcRef ref
  let wrappedType = foldl wrapTypeWithImplication (ctPred holeCt) imps
  --liftIO $ print $ map (showSDocUnsafe . ppr) djinnEnv
  -- liftIO $ print (showSDocUnsafe . ppr $ wrappedType)
  let splitSols = unwords . words . unwords . lines
      solToHf = RawHoleFit . parens . text
      Max numToShow = maxSols
  sols <- map splitSols <$> djinn True djinnEnv wrappedType maxSols microSecs
          -- We could set '-fdefer-typed-holes'  and load the module here...
          -- modInfo <- moduleInfo <$>
  let djinnSols = map solToHf $
                    take (numToShow `div` 10) $
                      sortOn length $ dedup sols
  return $ djinnSols <> fits
djinnSynthP _ _ _ = return []

-- Lazily de-duplicate a list
dedup :: Ord a => [a] -> [a]
dedup = dedup' Set.empty
  where dedup' sofar (x:xs) | x `Set.member` sofar = dedup' sofar xs
        dedup' sofar (x:xs) = x:dedup' (x `Set.insert` sofar) xs
        dedup' _ [] = []


data PluginType = Djinn 
                | Hoogle
                | Mod String
                deriving (Eq, Show)

toPluginType :: Maybe String -> [PluginType]
toPluginType (Just holeContent) = map toT spl
  where spl = split '&' holeContent
        toT c = case c of 
                "invoke djinn" -> Djinn
                "invoke hoogle" -> Hoogle
                'm':'o':'d':'u':'l':'e':' ':rest -> Mod rest
                _ -> error holeContent -- None
toPluginType _ = []


djinnHoogleModCP :: TcRef HolePluginState -> CandPlugin
djinnHoogleModCP ref hole cands =
  foldl (>>=) (return cands) $ map action $ toPluginType $ getHoleContent hole
     
     
  where greNotInOpts opts (GreHFCand gre) = not $ null $ intersect (inScopeVia gre) opts
        greNotInOpts _ _ = True
        inScopeVia = map (moduleNameString . importSpecModule) . gre_imp
        action ty cands = case ty of
                            -- Pass to the Djinn plugin
                            Djinn -> djinnAddToScopeP ref hole cands
                            -- Filter by where the elemnet comes from 
                            Mod modName -> return $ filter (greNotInOpts [modName]) cands
                            _ -> return cands


plugin :: Plugin
plugin = defaultPlugin { holeFitPlugin = holeFitP, pluginRecompile = purePlugin}

holeFitP :: [CommandLineOption] -> Maybe HoleFitPluginR
holeFitP opts = Just (HoleFitPluginR initP pluginDef stopP)
  where initP = initPlugin opts
        stopP = const $ return ()
        pluginDef ref = HoleFitPlugin { candPlugin = djinnHoogleModCP ref
                                      , fitPlugin  = djinnHoogleModFP ref }


getHoleContent :: TypedHole -> Maybe String
getHoleContent hole = 
    case tyHCt hole of
        Just (CHoleCan _ (ExtendedExprHole (ExtendedHole _ (Just (L _ str))))) -> Just $ unpackFS str
        Just (CHoleCan _ (ExtendedExprHole (ExtendedHoleSplice _ (L _ expr)))) -> 
            case expr of 
                HsPar _ (L _ (HsLit _ (HsString _ l))) -> Just $ unpackFS l
                _ -> Nothing
        _ -> Nothing

djinnHoogleModFP :: TcRef HolePluginState -> FitPlugin
djinnHoogleModFP ref hole hfs =
  foldl (>>=) (return hfs) $ map action $ toPluginType $ getHoleContent hole
  where action ty hfs  = case ty of
                           Djinn -> djinnSynthP ref hole hfs
                           Hoogle -> hoogleFP hole hfs
                           _ -> return hfs


searchHoogle :: String -> IO [String]
searchHoogle ty = lines <$> (readProcess "hoogle" [(show ty)] [])

hoogleFP :: FitPlugin
hoogleFP hole hfs =
    do dflags <- getDynFlags
       let tyString = showSDoc dflags . ppr . ctPred <$> tyHCt hole
       res <- case tyString of
                Just ty -> liftIO $ searchHoogle ty
                _ -> return []
       return $ (take 2 $ map (RawHoleFit . text . ("Hoogle: " ++)) res) ++ hfs
