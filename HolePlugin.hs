module HolePlugin where

import GhcPlugins

import GhcMonad

import TcHoleErrors

import Data.IORef

import TcRnMonad 

import Data.List (intersect, stripPrefix, find, sort)

--import TcRnTypes

import System.Process

import GHC

plugin :: Plugin
plugin = defaultPlugin { holeFitPlugin = hfp, pluginRecompile = purePlugin }

hfp :: [CommandLineOption] -> Maybe HoleFitPlugin
hfp opts = Just (HoleFitPlugin (candP opts) (fp opts))

data Filter = ByModule String | ByProp String

toFilter :: Maybe String -> Maybe Filter
toFilter (Just name)= case (stripPrefix "_module_" name) of
                        Just modl -> Just (ByModule modl)
                        _ -> case (stripPrefix "_satisfies_" name) of
                               Just prop -> Just (ByProp prop)
                               _ -> Nothing
toFilter _ = Nothing

replace :: Eq a => a -> a -> [a] -> [a]
replace match repl str = replace' [] str
  where replace' sofar (x:xs) | x == match = replace' (repl:sofar) xs
        replace' sofar (x:xs) = replace' (x:sofar) xs
        replace' sofar [] = reverse sofar


ghcInTcM :: Ghc a -> TcM a
ghcInTcM ghc = do env <- getTopEnv
                  liftIO $ do ref <- newIORef env
                              unGhc ghc (Session ref)

getProp :: String 
        -> [HoleFitCandidate]
        -> Maybe HoleFitCandidate
getProp propName cands = find ((==) propName . occNameString . occName) cands

-- | This candidate plugin filters the candidates by module,
--   using the name of the hole as module to search in
candP :: [CommandLineOption] -> CandPlugin
candP flags hole orig_cands =
  do (he, cands) <-
        case holeCt hole of
          Just ct@(CHoleCan _ h) ->
            do { let fakeCandOcc = mkVarOcc "fake_name"
               ; fakeCandName <- (flip mkSystemName fakeCandOcc) <$> getUniqueM
               ; let fakeCand = RawHFCand fakeCandName (text "fake candidate") (ctPred ct)
               ; return (Just (occNameString $ holeOcc h), (fakeCand:orig_cands)) }
          _ -> return (Nothing, orig_cands)
     liftIO $ print flags
     case toFilter he of
        Just (ByModule undscModName) -> do let replaced = replace '_' '.' undscModName
                                               res = filter (greNotInOpts [replaced]) cands
                                           return $ res
        Just (ByProp propName) -> do liftIO $ print propName
                                    
                                     test <- ghcInTcM $ do dflags <- getSessionDynFlags
                                                           ts <- getTargets
                                                           liftIO $ putStrLn $ showSDoc dflags (ppr ts)
                                                           let opts = pluginModNameOpts dflags
                                                               new = (GHC.mkModuleName "HolePluginArgs", "no-recur")
                                                           names <- getContext
                                                           liftIO $ putStrLn $ "names" ++ ( showSDoc dflags (ppr names))
                                                           dep <- (head . mgModSummaries) <$> getModuleGraph
                                                           liftIO $ putStrLn $ "deps" ++ ( showSDoc dflags (ppr dep))
                                                           let mname = (ms_mod_name $ dep)
                                                           checkLoad <- isLoaded mname
                                                           liftIO $ print $ "isLoaded: 1" ++ (show $ checkLoad)
                                                           let secondTime =  (new `elem` opts)
                                                           liftIO $ print secondTime
                                                           if secondTime
                                                           then do liftIO $ print $ "attempting parse"
                                                                   --res <- parseName propName
                                                                   return $ [] --"loaded:" ++ showSDoc dflags (ppr res)]
                                                           else do let ndflags = (dflags {pluginModNameOpts = (new:opts)})
                                                                   setSessionDynFlags ndflags --(gopt_set ndflags Opt_DeferTypedHoles)
                                                                   sesh <- hsc_HPT <$> getSession
                                                                   liftIO $ print $ (showSDoc dflags . pprHPT) $ sesh
                                                                   thisMod <- (parseModule dep) >>= typecheckModule >>= loadModule

                                                                   checkLoad <- isLoaded mname
                                                                   liftIO $ print $ "isLoaded 2:" ++ (show $ checkLoad)
                                                                   sesh2 <- hsc_HPT <$> getSession
                                                                   liftIO $ print $ (showSDoc dflags . pprHPT) $ sesh2
                                                                   res <- parseName "t"
                                                                   --con <- getContext
                                                                   --setContext (imp:con)
                                                                   --names <- getNamesInScope
                                                                   --let pnames = sort $ map (showSDoc dflags . ppr) names
                                                                   let topScope = sort $ map (showSDoc dflags . ppr) (modInfoExports $ moduleInfo $ thisMod)
                                                                   return topScope
                                     liftIO $ print test
                                     case getProp propName cands of
                                        Just (IdHFCand a)   -> do liftIO $ print "id cand"
                                                                  liftIO $ print $ (occNameString . occName) a
                                        Just (NameHFCand a) -> do liftIO $ print "name cand"
                                                                  liftIO $ print  $ (occNameString . occName) a
                                        Just (GreHFCand a)  -> do liftIO $ print "gre cand"
                                                                  liftIO $ print $ (occNameString . occName . gre_name) a
                                     return cands
        _ -> return cands
  where greNotInOpts opts (GreHFCand gre)  = not $ null $ intersect (inScopeVia gre) opts
        greNotInOpts _ _ = True
        inScopeVia = map (moduleNameString . importSpecModule) . gre_imp

-- Yes, it's pretty hacky, but it is just an example :)
searchHoogle :: String -> IO [String]
searchHoogle ty = lines <$> (readProcess "hoogle" [(show ty)] [])

fp :: [CommandLineOption] -> FitPlugin
fp flags hole hfs | "hoogle" `elem` flags =
    do dflags <- getDynFlags
       let tyString = showSDoc dflags . ppr . ctPred <$> holeCt hole
       res <- case tyString of
                Just ty -> liftIO $ searchHoogle ty
                _ -> return []
       return $ (take 2 $ map (RawHoleFit [] Nothing . text .("Hoogle says: " ++)) res) ++ hfs
fp _ _ hfs = return hfs
