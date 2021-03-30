module Main where

import CanonicalExpression

import System.IO
import Data.Time
import System.Exit
import System.Environment
---------------------------------------------------------
-- Several results connected with Zaions paradox
---------------------------------------------------------
main =
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
        hPutStr handler ("----------------\n\n" ++ (show startHour) ++ "\n")
        hPutStr handler (display  ++ "\n")
        finishHour <- getCurrentTime         
        hPutStr handler ((show finishHour) ++ "\n\n")        
        hClose handler
        
--------------------------------------------------------------------------
-- tracking tautologies that are not easy (size 100)
--------------------------------------------------------------------------
track =
  let sample = 10
      seed = 0
      size = 100
      limitMaxVar = 31
      l = someTaut seed size sample limitMaxVar
      display = "Propositions that are tautologies but not simple intuitionistic:\n" ++
                show (lBT2LBT l) ++"\nsize = " ++ show size ++
                "\nsample = " ++ show sample ++ "\nseed = " ++ show seed ++ "\nnumber of propositions = " ++ show (length l)
  in do startHour <- getCurrentTime
        handler <- openFile "./DATA/Tautologies.txt" AppendMode
        hPutStr handler ("----------------\n\n" ++ (show startHour) ++ "\n")
        hPutStr handler (display  ++ "\n")
        finishHour <- getCurrentTime         
        hPutStr handler (show "start time  " ++ (show startHour) ++ "\n")        
        hPutStr handler (show "finish time " ++ (show finishHour) ++ "\n\n")        
        hClose handler   
