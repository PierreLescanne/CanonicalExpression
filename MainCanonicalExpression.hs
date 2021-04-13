module Main where

import CanonicalExpression

import System.IO
import Data.Time
import System.Exit
import System.Environment
---------------------------------------------------------
-- Several results connected with Zaions paradox
---------------------------------------------------------
stats =
  let limitMaxVar = 31 {-- the index of the large variable --}
      seed = 0
      size = 100  {-- size of the formulas --}
      sample = 1000  {-- size of the sample --}
      
      (nbSimpInt,nbTaut,ratio,tauts,bigs,tooBig) = zaionc limitMaxVar 0 size sample
      display = "for size " ++ show size ++ ", seed " ++ show seed ++
                ", largest variable " ++ show limitMaxVar ++
                " and sample size " ++ show sample ++ "\n" ++
                "yields " ++ show nbSimpInt ++
                " intuitionistic propositions,\n" ++
                "and " ++ show nbTaut ++ " tautologies, namely\n\n" ++ 
                show (lBT2LBT tauts) ++
                "ratio My-Intuitionistic/Tautologies is\n " ++
                show ratio ++ "\n\n" ++
                show tooBig ++ " formulas were considered as too big" ++ " namely\n\n" ++
                show (lBT2LBT bigs)
  in do startHour <- getCurrentTime
        handler <- openFile "./DATA/RatioIntuitionisticVsTautology.txt" AppendMode
        hPutStr handler ("----------------\n\n")
        hPutStr handler (display  ++ "\n")
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
  let szS = 800
      seed = 0
      size = 25
      aCE seed = canExp2LBT $ aCanExp seed size
      nbSimpleS = generateAndCount seed szS aCE isSimple 
      ratio = (fromIntegral nbSimpleS) / (fromIntegral szS)
      display = "The ratio of simple expressions over all expressions is " ++
                show ratio ++
                "\nseed = " ++ show seed ++
                "\nexpression size  = " ++ show size ++      
                "\nsample size = " ++ show szS ++
                "\nnumber of simple exps found = " ++ show nbSimpleS
  in do startHour <- getCurrentTime
        handler <- openFile "./DATA/RatioSimple.txt" AppendMode
        hPutStr handler ("----------------\n\n")
        hPutStr handler (display  ++ "\n")
        finishHour <- getCurrentTime         
        hPutStr handler ("start time  " ++ (show startHour) ++ "\n")        
        hPutStr handler ("finish time " ++ (show finishHour) ++ "\n\n")
        hClose handler
--------------------------------------------------
-- Ratio simple over non classical simple
--------------------------------------------------
-- ratioSimplNonClassSimp =
main =
  let szS = 400
      seed = 0
      size = 25
      aCE seed = canExp2LBT $ aCanExp seed size
      (nbSimp,nbNonSimpNonTaut) = genitrini  seed size szS
      ratio = (fromIntegral nbSimp) / (fromIntegral (nbSimp + nbNonSimpNonTaut))
      display = "seed = " ++ show seed ++
                "\nexpression size = " ++ show size ++
                "\nsample size = " ++ show szS ++ "\n-----" ++
                "\nnumber of simples = " ++ show nbSimp ++
                "\nnumber of non simple non tautologies = " ++ show nbNonSimpNonTaut ++
                "\n-----" ++
                "\nThe ratio simple over non simple non tautologies is " ++ show ratio
  in do startHour <- getCurrentTime
        handler <- openFile "./DATA/RatioNotNonCLassSimpl.txt" AppendMode
        hPutStr handler ("-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+\n\n")
        hPutStr handler (display  ++ "\n")
        finishHour <- getCurrentTime
        hPutStr handler ("-----" ++ "\nstart time:  " ++ (show startHour) ++ "\n")
        hPutStr handler ("finish time: " ++ (show finishHour) ++ "\n\n")
        hClose handler 
                
