module Main where

import CanonicalExpression

import System.IO
import Data.Time
import System.Exit
import System.Environment
import Network.HostName

---------------------------------------------------------
-- Several results connected with Zaionc paradox
---------------------------------------------------------
-- zaionc =
main =
  let limitMaxVar = 31 {-- the index of the largest variable --}
      seed = 1
      size =50  {-- size of the expressions --}
      sampleSize = 200  {-- size of the sample --}
      (nbCheap,nbTaut,ratio,tauts,bigs,nbBigs) = zaionc limitMaxVar 0 size sampleSize
      displayInit = "ratio Cheap/Classical\n" ++
                    "seed = " ++ show seed ++
                    "\nexpression size = " ++ show size ++
                    "\nsample size = " ++ show sampleSize ++
                    "\nlargest variable = " ++ show limitMaxVar ++ "\n-----"
      display = "yields " ++ show nbCheap ++
                " cheap propositions,\n" ++
                "and " ++ show nbTaut ++ " not intuitionistic tautologies, namely\n\n" ++ 
                show (lBT2LBT tauts) ++
                "ratio Cheap/Tautologies is\n " ++
                show ratio ++ "\n\n" ++
                show nbBigs ++ " formulas were considered as too big, but potential tautologies" ++ " namely\n\n" ++
                show (lBT2LBT bigs)
  in do startHour <- getCurrentTime
        hostName <- getHostName
        handlerLog <- openFile "./DATA/log.txt" AppendMode
        hPutStr handlerLog ("\non " ++ hostName ++ "\nat " ++ (show startHour) ++ "\n" ++ displayInit)
        hClose handlerLog
        handler <- openFile "./DATA/RatioIntuitionisticVsTautology.txt" AppendMode
        hPutStr handler ("----------------\n\n")
        hPutStr handler ("on " ++ hostName ++ "\n" ++ displayInit ++ "\n" ++ display  ++ "\n")
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
  let sampleSize = 800
      seed = 0
      size = 25
      aCE seed = canExp2LBT $ aCanExp seed size
      nbSimpleS = generateAndCount seed sampleSize aCE isSimple 
      ratio = (fromIntegral nbSimpleS) / (fromIntegral sampleSize)
      displayInit = "ratioSimpl\n" ++
                    "seed = " ++ show seed ++
                    "\nexpression size = " ++ show size ++
                    "\nsample size = " ++ show sampleSize ++ "\n-----\n"
      display = displayInit ++ "The ratio of simple expressions over all expressions is " ++
                show ratio ++
                "\nseed = " ++ show seed ++
                "\nexpression size  = " ++ show size ++      
                "\nsample size = " ++ show sampleSize ++
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
--      aCE seed = canExp2LBT $ aCanExp seed size -- a random canonical expression
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
                
