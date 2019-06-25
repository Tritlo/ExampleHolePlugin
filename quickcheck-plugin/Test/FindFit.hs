{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
module Test.FindFit where

import Text.Printf
import Test.ProgInput
import Data.List (nub, intercalate, replicate)

getProgInputs :: FilePath -> IO [ProgInput]
getProgInputs file = map (read @ProgInput) . lines <$> readFile file

data ProgOut = ProgO { oHoleL :: Maybe String
                     , oHoleN :: Maybe String
                     , oModN :: String
                     , fits :: [String] } deriving (Show, Read)
                      

genFitTestModule :: IO ()
genFitTestModule = do pins <- getProgInputs "out.fits"
                      let mods = map modN pins
                      print pins
                      let prel = unlines $ genProgPrelude mods
                      let fr = genFitReps $ map piToChecks pins
                      let prog = prel ++ fr ++ genMain
                      putStrLn $ prog
                      writeFile "FitTest.hs" prog

addToFile :: ProgOut -> IO ()
addToFile (ProgO { oHoleL = Just filename
                 , oHoleN = Just hn
                 , fits = fits})
    = appendFile filename (printf "%s = %s\n" hn res)
         where res = case fits of
                      [] -> "undefined"
                      (f:_) -> f
addToFile _ = return ()

addToFiles :: [ProgOut] -> IO ()
addToFiles = mapM_ addToFile



genMain :: Program
genMain = unlines $ ["", "executeFitTest = fitReps >>= addToFiles"]

genFitReps :: [Statement] -> Program
genFitReps checks = unlines ["fitReps :: IO [ProgOut]"
                           , "fitReps = " ++ toSeq checks] 

piToChecks :: ProgInput -> Statement
piToChecks (ProgIn {..}) = (genChecks propN fitStrs)
                        ++  (printf ">>= (\\r -> return (ProgO {oHoleL = %s, oHoleN = %s, oModN = %s, fits = r}))" (show holeL) (show holeN) (show modN))

                              
type PropName = String
type ModName = String
type Fit = String
type Program = String
type Statement = String

genProgPrelude :: [ModName] -> [Statement]
genProgPrelude mods = [ "module FitTest where"
                      , "import Test.QuickCheck"
                      , "import Test.FindFit"]
                      ++ (map (printf "import %s") ( nub mods))
                      ++ [""]

toSeq :: [Statement] -> Statement
toSeq stmts = "(sequence [" ++ (intercalate "," stmts) ++ "])"

genChecks :: PropName -> [Fit] -> Statement
genChecks propName fits = "( (map fst . filter (\\(_,r) -> r)) <$> "
                       ++ (toSeq $  map check fits) ++ ")"
 where 
   check :: String -> String
   check fit = printf "(\\r -> (\"%s\", isSuccess r)) <$> (quickCheckResult (%s %s))" fit propName fit



