module Main where

import CanonicalExpression

import System.IO
import Data.Time
import System.Exit
import System.Environment
import Network.HostName

---------------------------------------------------------
-- Several results connected with Zaions paradox
---------------------------------------------------------
-- ratioCheapClass =
main =
  let limitMaxVar = 31 {-- the index of the large variable --}
      seed = 0
      size = 100  {-- size of the formulas --}
      sampleSize = 10  {-- size of the sample --}
      
      (nbSimpInt,nbTaut,ratio,tauts,bigs,tooBig) = zaionc limitMaxVar 0 size sampleSize
      displayInit = "ratioCheapClass\n" ++
                    "seed = " ++ show seed ++
                    "\nexpression size = " ++ show size ++
                    "\nsample size = " ++ show sampleSize ++
                    "\nlargest variable = " ++ show limitMaxVar ++ "\n-----"
      display = "yields " ++ show nbSimpInt ++
                " intuitionistic propositions,\n" ++
                "and " ++ show nbTaut ++ " tautologies, namely\n\n" ++ 
                show (lBT2LBT tauts) ++
                "ratio My-Intuitionistic/Tautologies is\n " ++
                show ratio ++ "\n\n" ++
                show tooBig ++ " formulas were considered as too big" ++ " namely\n\n" ++
                show (lBT2LBT bigs)
  in do startHour <- getCurrentTime
        hostName <- getHostName
        handlerLog <- openFile "./DATA/log.txt" AppendMode
        hPutStr handlerLog ("\non " ++ hostName ++ "\nat " ++ (show startHour) ++ "\n" ++ displayInit)
        hClose handlerLog
        handler <- openFile "./DATA/RatioIntuitionisticVsTautology.txt" AppendMode
        hPutStr handler ("----------------\n\n")
        hPutStr handler ("on " ++ hostName ++ "\n" ++ display  ++ "\n")
        finishHour <- getCurrentTime         
        hPutStr handler ("start hour = " ++ (show startHour) ++ "\n\n")        
        hPutStr handler ("finish hour = " ++ (show finishHour) ++ "\n\n")        
        hClose handler
        
--------------------------------------------------------------------------
-- tracking tautologies that are not easy (size 100)
--------------------------------------------------------------------------
track =
  let seed = 0
      size = 100
      sample = 10
      limitMaxVar = 31
      l = someTaut seed size sample limitMaxVar
      display = "Propositions that are tautologies but not simple intuitionistic:\n" ++
                show (lBT2LBT l) ++"\nsize = " ++ show size ++
                "\nsample = " ++ show sample ++ "\nseed = " ++ show seed ++ "\nnumber of propositions = " ++ show (length l)
  in do startHour <- getCurrentTime
        handler <- openFile "./DATA/Tautologies.txt" AppendMode
        hPutStr handler ("----------------\n\n")
        hPutStr handler (display  ++ "\n")
        finishHour <- getCurrentTime         
        hPutStr handler ("start time  " ++ (show startHour) ++ "\n")        
        hPutStr handler ("finish time " ++ (show finishHour) ++ "\n\n")        
        hClose handler   

--------------------------------------------------
-- Ratio simple over all
--------------------------------------------------
ratioSimpl =
-- main =
  let sampleSize = 10
      seed = 0
      size = 2000
      sample = [canExp2LBT$aCanExp sid size | sid<- [seed..(seed+(sampleSize - 1))]]
      nbSimpleS = length $ filter isSimple sample
      ratio = (fromIntegral nbSimpleS) / (fromIntegral sampleSize)
      displayInit = "ratioSimpl\n" ++
                    "seed = " ++ show seed ++
                    "\nexpression size = " ++ show size ++
                    "\nsample size = " ++ show sampleSize ++ "\n-----\n"
      display = displayInit ++ "The ratio of simple expressions over all expressions is " ++
                show ratio ++
                "\nnumber of simple exps found = " ++ show nbSimpleS
  in do startHour <- getCurrentTime
        hostName <- getHostName
        handlerLog <- openFile "./DATA/log.txt" AppendMode
        hPutStr handlerLog ("\non " ++ hostName ++ "\nat " ++ (show startHour) ++ "\n" ++ displayInit)
        hClose handlerLog
        handler <- openFile "./DATA/RatioSimple.txt" AppendMode
        hPutStr handler ("----------------\n\n")
        hPutStr handler (display  ++ "\n")
        finishHour <- getCurrentTime         
        hPutStr handler ("start time  " ++ (show startHour) ++ "\n")        
        hPutStr handler ("finish time " ++ (show finishHour) ++ "\n\n")
        hClose handler
--------------------------------------------------
-- Ratio simples over non simple antilogies
--------------------------------------------------
ratioSimplNonClassSimp =
-- main =
  let szS = 100
      seed = 0
      size = 25
      (nbSimp,nbNonSimpNonTaut) = genitrini  seed size szS
      ratio = (fromIntegral nbSimp) / (fromIntegral (nbSimp + nbNonSimpNonTaut))
      displayInit = "ratioSimplNonSimpAnt\n" ++
                    "seed = " ++ show seed ++
                    "\nexpression size = " ++ show size ++
                    "\nsample size = " ++ show szS ++ "\n-----"
      display = displayInit ++
                "\nnumber of simples = " ++ show nbSimp ++
                "\nnumber of non simple non tautologies = " ++ show nbNonSimpNonTaut ++
                "\n-----" ++
                "\nThe ratio simple over non simple non tautologies is " ++ show ratio
  in do startHour <- getCurrentTime
        hostName <- getHostName
        handlerLog <- openFile "./DATA/log.txt" AppendMode
        hPutStr handlerLog ("\non " ++ hostName ++ "\nat " ++ (show startHour) ++ "\n" ++ displayInit)
        hClose handlerLog
        handler <- openFile "./DATA/RatioNotNonCLassSimpl.txt" AppendMode
        hPutStr handler ("-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+\n\n")
        hPutStr handler ("on " ++ hostName ++ "\n" ++ display  ++ "\n")
        finishHour <- getCurrentTime
        hPutStr handler ("-----" ++ "\nstart time:  " ++ (show startHour) ++ "\n")
        hPutStr handler ("finish time: " ++ (show finishHour) ++ "\n\n")
        hClose handler 
                
