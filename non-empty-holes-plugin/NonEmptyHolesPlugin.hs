{-# LANGUAGE TypeApplications, RecordWildCards, DeriveDataTypeable, MagicHash #-}
module NonEmptyHolesPlugin where

import GhcPlugins hiding ((<>), getSrcSpanM)

import TcHoleErrors


import TcRnTypes

import TcRnMonad

import DjinnBridge

import ConLike(conLikeWrapId_maybe)
import TcEnv (tcLookup)
import Data.Maybe (catMaybes)

import Data.List (sortOn)

import qualified Data.Set as Set

import Data.List (intersect)

import System.Process

import HsExpr

import Language.Haskell.TH hiding (ppr, Type, Name)

import Data.Data

import Data.Char (isSpace)

import HsExtension (GhcTc, GhcPs)
import Data.Dynamic

import qualified Control.Monad.State.Lazy as St
import Control.Monad.State.Lazy (State)

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import Data.Foldable (toList)

import Language.Haskell.TH.Syntax (liftData, unsafeTExpCoerce)


type Cmd = State (Seq PluginType)

invoke :: PluginType -> Cmd ()
invoke t = St.modify (flip (Seq.|>) t)

filterBy :: String -> Cmd ()
filterBy str = St.modify (flip (Seq.|>) (Mod str))

boo :: Bool -> Cmd Bool
boo = return

pfp :: String -> Cmd ()
pfp = error

execTyped :: Cmd () -> Q (TExp [PluginType])
execTyped cmds = unsafeTExpCoerce $ liftData $ toList $ St.execState cmds Seq.empty

exec :: Cmd () -> Q Exp
exec cmds = unType <$> (execTyped cmds)


data HolePluginState = HPS { djinnEnv :: Environment
                           , maxSols :: MaxSolutions
                           , microSecs :: Int}

setDjinnEnv :: Environment -> HolePluginState -> HolePluginState
setDjinnEnv e (HPS _ sols secs)  = HPS e sols secs

initPlugin :: [CommandLineOption] -> TcM (TcRef HolePluginState)
-- We take more than we need since djinn is prone to duplicate solutions...
initPlugin _ = newTcRef $ HPS [] (Max 20) (40000 :: Int)

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse .  dropWhile isSpace


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
                | Discard
                deriving (Eq, Show, Data, Typeable)

toPluginType :: Maybe String -> [PluginType]
toPluginType (Just holeContent) = map toT spl
  where spl = split '&' holeContent
        toT c = case (trim c) of
                "invoke djinn" -> Djinn
                "invoke hoogle" -> Hoogle
                'f':'i':'l':'t':'e':'r':'B':'y':' ':rest -> Mod rest
                _ -> error $ show $ trim c
toPluginType _ = []



getCommands :: TypedHole -> TcM [PluginType]
getCommands hole = recoverM (return [Discard]) $ do
                    case hexpr of
                        Just (Left lexpr) ->
                          do dv <- runEHRExprDyn lexpr
                             return $ fromDyn dv (error $ show $ dynTypeRep dv ) -- showSDocUnsafe $ ppr lexpr) 
                        Just (Right lexpr) ->
                          do dv <- runEHSplice lexpr
                             return $ fromDyn dv (error $ showSDocUnsafe $ ppr lexpr) 
                        _ -> return []
 where hexpr = getHoleExpr hole

djinnHoogleModCP :: TcRef HolePluginState -> CandPlugin
djinnHoogleModCP ref hole candidates = do
  commands <- getCommands hole
  foldl (>>=) (return candidates) $ map action commands
  where greNotInOpts opts (GreHFCand gre) = not $ null $ intersect (inScopeVia gre) opts
        greNotInOpts _ _ = True
        inScopeVia = map (moduleNameString . importSpecModule) . gre_imp
        action ty cands = case ty of
                            -- Pass to the Djinn plugin
                            Djinn -> djinnAddToScopeP ref hole cands
                            -- Filter by where the elemnet comes from 
                            Mod modName -> return $ filter (greNotInOpts [modName]) cands
                            Discard -> return []
                            _ -> return cands

plugin :: Plugin
plugin = defaultPlugin { holeFitPlugin = holeFitP, pluginRecompile = purePlugin}

holeFitP :: [CommandLineOption] -> Maybe HoleFitPluginR
holeFitP opts = Just (HoleFitPluginR initP pluginDef stopP)
  where initP = initPlugin opts
        stopP = const $ return ()
        pluginDef ref = HoleFitPlugin { candPlugin = djinnHoogleModCP ref
                                      , fitPlugin  = djinnHoogleModFP ref }

getHoleExpr :: TypedHole -> Maybe (Either (LHsExpr GhcPs) (LHsExpr GhcTc))
getHoleExpr hole = 
    case tyHCt hole of
        Just (CHoleCan _ (NonEmptyExprHole _ (EHRExpr e))) -> Just $ Left e
        Just (CHoleCan _ (NonEmptyExprHole _ (EHRSplice spl))) -> Just $ Right spl
        _ -> Nothing

djinnHoogleModFP :: TcRef HolePluginState -> FitPlugin
djinnHoogleModFP ref hole fits =
  do commands <- getCommands hole
     foldl (>>=) (return fits) $ map action commands
  where action ty hfs  = case ty of
                           Djinn -> djinnSynthP ref hole hfs
                           Hoogle -> hoogleFP hole hfs
                           Discard -> return []
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
