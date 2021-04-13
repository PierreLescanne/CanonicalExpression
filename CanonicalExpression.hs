module CanonicalExpression where

import Data.List
import Data.Maybe
import Text.Show
import Control.Monad.State
import System.Random
import Constants

-- given n, yields a stream of Int's between 0 and n inclusive
randomInt :: Int -> [Int]
randomInt n = randomRs (0,n-1) (mkStdGen generatorStarter)
              {- generatorStarter in Constants.hs -}

-- ========= Cartesian product ==========
x :: [a] -> [b] -> [(a,b)]
x [] l2 = []
x (x1 : l1) l2 = map ((,) x1) l2 ++ (l1 `x` l2)

-- ========== Strings of naturals ==========
-- strings n p : the stream of strings on [0..p] of size n
strings :: Int -> Int -> [[Int]]
strings 0 _ = [[]]
strings n p = foldl (++) [] [(map ((:) i) (strings (n-1) p)) | i <- [0..p]]

-- a random string of a given size on [0..p]
aString :: Int -> Int -> Int -> [Int]
aString _ 0 _ = []
aString _ sz 0 = take sz [0..]
aString seed sz p = randomInt p !! seed : aString (seed + 1) (sz-1) p

-- ========== Catalan and binary trees ==========
-- ~~~~ count binary trees ~~~~
catalan :: [Integer]
catalan = 1 : [let cati = take n catalan in sum (zipWith (*) cati (reverse cati)) | n <- [1..]]
 
-- ~~~~ generate all ~~~~
data BinTree = Empty | Node BinTree BinTree

instance Show BinTree where
  show Empty  = "□"
  show (Node t1 t2) = "(" ++ show t1 ++ "→" ++ show t2 ++ ")"

-- the stream of binary trees aka parenthesised expressions
binTrees :: [[BinTree]]
binTrees = [Empty] : [let bTi = take n binTrees
                      in map (\(b1,b2) -> Node b1 b2) (foldl (++) [] (zipWith x bTi (reverse bTi)))
                     | n <- [1..]]
-- ~~~~ generate random
-- generate a random binary tree by Rémy's algorithm
type Gen = State StdGen

-- getting a random value between 0 and 4n-3 inclusive
randFR :: Int -> Gen Int
randFR n = do generator <- get
              let (value, newGenerator) = randomR (0,4*n-3) generator
              put newGenerator
              return value

randForRemy :: Int -> Int -> Int
randForRemy seed n = evalState (randFR n) (mkStdGen ((seed+1)*n))


-- a Random Binary Tree coded as a List
-- (external nodes are even numbers, internal nodes are odd numbers)
-- (see Knuth Algorithm R, in TAOCP vol4A, fasicle 4, § 7.2.1.6, page 19)
rbtL :: Int -> Int -> [Int]
rbtL seed 0 = [0]
rbtL seed n =
  let x = randForRemy seed n
      l = rbtL seed (n-1)
      b = even x
      k = x `div` 2
      l2n1 = if b then l !! k else 2*n
      l2n = if b then 2*n else l !! k
  in take k l ++ [2*n-1] ++ drop (k+1) l ++ [l2n1,l2n]

-- decode a list l representing a binary tree, starting at index n
decodeBT :: Int -> [Int] -> BinTree 
decodeBT n l = if even n then Empty
                         else Node (decodeBT (l !! n) l)
                                   (decodeBT (l !! (n+1)) l)
-- a random binary tree
aBinTree :: Int -> Int -> BinTree
aBinTree seed sz = let l = rbtL seed sz
                   in decodeBT (l!!0) l

-- ========== Stirling numbers and partitions ==========
-- ~~~~ count ~~~~
-- numbers of 'restricted growth strings' of size n limited to the index $i$
-- aka Stirling partition numbers or Stirling numbers of the second kind
-- i.e., numbers of partitions of an n-set into i blocks
stirl_tab :: [[Integer]]
stirling :: Int -> Int -> Integer

stirl_tab = [[stirling i n | n <- [0..]] | i <- [0..]]

stirling 0 0 = 1
stirling 0 n = 0
stirling i 0 = 0
stirling i n = sum (zipWith (*) [toInteger (i ^ j) | j <- [0..(n-1)]]
                         (reverse (take n (stirl_tab !! (i - 1)))))
-- ~~~~ generate all ~~~~
-- 'restricted growth string' (Knuth vol. 4, fasc. 3, § 7.2.1.5, p. 62)
-- 'irregular staircase', Flajolet Sedgewick p. 62-63

-- 0 on the left
-- (i,n)-strings for (i,n)-partitions
-- N.B. This is the most usual presentation (Knuh, Flajolet and Sedgewick)
restGrStrsL_tab :: [[[[Int]]]]
restGrStrsL :: Int -> Int -> [[Int]]

restGrStrsL_tab = [[restGrStrsL i n | n <- [0..]] | i <- [0..]]

restGrStrsL 0 0 = [[]]
restGrStrsL 0 n = []
restGrStrsL i 0 = []
restGrStrsL i n =
  let csi = take n $ restGrStrsL_tab !! (i-1)
      strs = reverse [strings j (i-1) | j <- [0..(n-1)]]
  in map (\(w,s) -> w ++ [i-1] ++ s) (foldr (++) [] (zipWith x csi strs))

-- 0 on the right,
-- for instance [7,1,3,4,4,0,0,2,4,6,3,6,4,6,0,5,4,4,4,3,2,1,0,0,0]
-- N.B. We are interested in 0 on the right.
restGrStrsR_tab :: [[[[Int]]]]
restGrStrsR :: Int -> Int -> [[Int]]

restGrStrsR_tab = [[restGrStrsR i n | n <- [0..]] | i <- [0..]]

restGrStrsR 0 0 = [[]]
restGrStrsR 0 n = []
restGrStrsR i 0 = []
restGrStrsR i n = 
  let csi = take n $ restGrStrsR_tab !! (i-1)
      strs = [strings (n-1-j)  (i-1) | j <- [0..(n-1)]]
  in map (\(w,s) -> s ++ [i-1] ++ w) (foldr (++) [] (zipWith x csi strs))

-- ========== Bell numbers and partitions ==========
-- ~~~~ count ~~~~
-- Bell numbers
bell :: Int -> Integer
bell n = sum [stirling i n | i <- [0..n]]
bell_tab = [bell n | n <- [0..]]

-- ~~~~ generate all set-partitions ~~~~
-- partitions in an n-set
partitionsL :: Int -> [[Int]]
partitionsL n = foldl (++) [] [restGrStrsL i n | i <- [0..n]]

partit_tab :: [[[Int]]]
partit_tab = [partitionsL n | n <- [0..]]

-- from right to left
partitionsR :: Int -> [[Int]]
partitionsR n = foldl (++) [] [restGrStrsR i n | i <- [0..n]]

-- ~~~~ generate random partitions ~~~~
-- pedestrian and unefficient method
-- given a seed and a size, returns a random canonical string of that size
aRestGrStr :: Int -> Int -> [Int]
aRestGrStr seed sz = partitionsR sz !! (fromIntegral $ randomInt (fromIntegral $ (bell sz)) !! seed)
-- limited to 12

-- ~~~~ Stam algorithm ~~~~
-- (see Knuth Algorithm R, in TAOCP vol4A, fasicle 3, § 7.2.1.5, page 74)
-- values of probForM10, probForM25, probForM100 etc. are in Constants.hs
-- They are computed elsewhere (SageMath) and patched here?
accu :: Num a => [a] -> [a]
accu [] = []
accu (x : l) = x : map ((+) x) (accu l)

-- the list is sorted and values are betewwen 0.0 and 1.0
-- given v, look for the first index i, s.t. l!!i bounds v,
-- i.e., l!!(i-1) <= v <  l!!i
look :: Double -> [Double] -> Int -> Int
look _ [] i = i
look v (x : l) i = if v < x then (i -1) else look v l (i+1)

-- getting a random value betwenn 0 and 1
rand :: Gen Double
rand = do generator <- get
          let (value, newGenerator) = randomR (0,1) generator
          put newGenerator
          return value

-- a random M as in Knuth for n = 10
accuProbForM10 :: [Double]
accuProbForM10 = accu probForM10

aM10 :: Int -> Int
aM10 seed = let randomM10 :: Gen Int
                randomM10 =
                  do randomDouble <- rand
                     return (look randomDouble accuProbForM10 0)
            in evalState randomM10 (mkStdGen seed)

-- a random M as in Knuth for n = 25
accuProbForM25 :: [Double]
accuProbForM25 = accu probForM25

aM25 :: Int -> Int
aM25 seed = let randomM25 :: Gen Int
                randomM25 = 
                  do randomDouble <- rand
                     return (look randomDouble accuProbForM25 0)
            in evalState randomM25 (mkStdGen seed)

-- a random M as in Knuth for n = 50
accuProbForM50 :: [Double]
accuProbForM50 = accu probForM50

aM50 :: Int -> Int
aM50 seed = let randomM50 :: Gen Int
                randomM50 = 
                  do randomDouble <- rand
                     return (look randomDouble accuProbForM50 0)
            in evalState randomM50 (mkStdGen seed)

-- a random M as in Knuth for n = 100
accuProbForM100 :: [Double]
accuProbForM100 = accu probForM100

aM100 :: Int -> Int
aM100 seed = let randomM100 :: Gen Int
                 randomM100 =
                   do randomDouble <- rand
                      return (look randomDouble accuProbForM100 0)
             in evalState randomM100 (mkStdGen seed)

-- a random M as in Knuth for n = 500
accuProbForM500 :: [Double]
accuProbForM500 = accu probForM500

aM500 :: Int -> Int
aM500 seed = let randomM500 :: Gen Int
                 randomM500 =
                   do randomDouble <- rand
                      return (look randomDouble accuProbForM500 0)
             in evalState randomM500 (mkStdGen seed)

-- a random M as in Knuth for n = 1000
accuProbForM1000 :: [Double]
accuProbForM1000 = accu probForM1000

aM1000 :: Int -> Int
aM1000 seed = let randomM1000 :: Gen Int
                  randomM1000 =
                    do randomDouble <- rand
                       return (look randomDouble accuProbForM1000 0)
              in evalState randomM1000 (mkStdGen seed)
                
-- make a 'restricted growth string' from a 'class description'
-- `rgs` is an accumulator for the result, initially `rgs` contains only the value -1
-- `l` is the list of 'a class description'
-- `i` is the number of the class
howToRename :: [Int] -> Int -> [Int] -> [Int]
howToRename rgs _ [] = rgs
howToRename rgs i (j:l) =
  if (rgs !! j /= -1) then howToRename rgs i l  -- do nothing
  else howToRename ((take j rgs) ++ [i] ++ (drop (j+1) rgs)) (i+1) l -- assign i to rgs!!j

rename :: [Int]  -> [Int] -> [Int]
rename rgs [] = []
rename rgs (i:l) = (rgs !! i) : (rename rgs l)

-- 0 must be on the right, hence reverse
mkRGS :: [Int] -> [Int]
mkRGS l = reverse $ rename (howToRename (take (length l) [-1,-1..]) 0 l) l

--
aRestGrStr10 :: Int -> [Int]
aRestGrStr10 seed = mkRGS $ aString seed 10 (aM10 (-seed))

aRestGrStr25 :: Int -> [Int]
aRestGrStr25 seed = mkRGS $ aString seed 25 (aM25 (-seed))

aRestGrStr50 :: Int -> [Int]
aRestGrStr50 seed = mkRGS $ aString seed 50 (aM50 (-seed))

aRestGrStr100 :: Int -> [Int]
aRestGrStr100 seed = mkRGS $ aString seed 100 (aM100 (-seed))

aRestGrStr500 :: Int -> [Int]
aRestGrStr500 seed = mkRGS $ aString seed 500 (aM500 (-seed))

aRestGrStr1000 :: Int -> [Int]
aRestGrStr1000 seed = mkRGS $ aString seed 1000 (aM1000 (-seed))

-- ========== Canonical expressions ==========
-- ~~~~ count ~~~~
-- The sequence of the numbers of canonical expressions A289679,
-- catalan (n-1) * Bell (n)
-- NB. countCanExps !!17 = 87870884165207473770 on arch32
-- A289679 18 = 88427704298354261610 (agrees with sage on arch64)
countCanExps :: [Integer]
countCanExps = zipWith (*) catalan (tail bell_tab)

-- ~~~~ generate ~~~~
-- == The data type of canonical expressions ==
data CanExp = Pair BinTree [Int]

instance Show CanExp where
  show (Pair bt s)  = let print (Empty , n : l) = Just (("α" ++ show n) , l)
                          print (Empty , []) = Nothing
                          print (Node t1 t2 , l) =
                            case print (t1,l) of
                              Nothing -> Nothing
                              Just (s1 , l1) -> case print (t2,l1) of
                                Nothing -> Nothing
                                Just (s2 , l2) -> Just ("(" ++ s1 ++ "→" ++ s2 ++ ")",l2)
                      in case print (bt,s) of
                           Nothing -> "The string is too short"
                           Just (s,l) -> case l of
                               [] -> s
                               _ -> "The string is too long"

-- To show nicely lists of CandExp
data ListCanExp = EmptyLCE | ConsLCE CanExp ListCanExp

instance Show ListCanExp where
  show EmptyLCE = ""
  show (ConsLCE e l) = show e ++ "\n\n" ++ show l

lCE2LCE :: [CanExp] -> ListCanExp
lCE2LCE [] = EmptyLCE
lCE2LCE (e : l) = ConsLCE e (lCE2LCE l)

-- the stream of canonical expressions by size
-- x0 on the rightmost position
canonicalExpsOf :: Int -> [CanExp]
canonicalExpsOf n = map (\(bt,p) -> Pair bt p) ((binTrees !!n) `x` (partitionsR (n+1)))

canonicalExps :: [[CanExp]]
canonicalExps = [canonicalExpsOf n | n<-[0..]]

-- Pierce formula
pierce :: CanExp
pierce = Pair (Node (Node (Node Empty Empty) Empty) Empty) [0,1,0,0]

-- == The data type of labeled binary tree ==
data LabBinTree = Leaf Int | LNode LabBinTree LabBinTree

instance Show LabBinTree where
  show (Leaf n) = "x" ++ show n
  show (LNode lbt1 lbt2) = "(" ++ show lbt1 ++ "→" ++ show lbt2 ++ ")"

-- To show nicely lists of CandExp
data ListLBT = EmptyLBT | ConsLBT LabBinTree ListLBT

instance Show ListLBT where
  show EmptyLBT = ""
  show (ConsLBT e l) = show e ++ "\n\n" ++ show l

lBT2LBT :: [LabBinTree] -> ListLBT
lBT2LBT [] = EmptyLBT
lBT2LBT (e : l) = ConsLBT e (lBT2LBT l)

-- translations of 'canonical expression' to a 'labeled  binary tree'
canExp2LBT :: CanExp -> LabBinTree
canExp2LBT (Pair bt s) = let cE2LBT :: (BinTree,[Int]) -> Maybe (LabBinTree,[Int])
                             cE2LBT (Empty , n : l) = Just (Leaf n, l)
                             cE2LBT (Empty , []) = Nothing
                             cE2LBT (Node t1 t2 , l) =
                               case cE2LBT (t1,l) of
                                 Nothing -> Nothing
                                 Just (lbt1 , l1) -> case cE2LBT (t2,l1) of
                                   Nothing -> Nothing
                                   Just (lbt2 , l2) -> Just (LNode lbt1 lbt2,l2)
                         in case cE2LBT (bt,s) of
                              Nothing -> Leaf 4242
                              Just (lbt,[]) -> lbt
                              Just (_,_:_) -> Leaf 314


-- =====================================================
-- Generating random canonical expressions
-- =====================================================

-- ~~~~ random generation ~~~~

aCanExp10 :: Int -> CanExp
aCanExp10 seed = Pair (aBinTree seed 9) (aRestGrStr10 seed)

aCanExp25 :: Int -> CanExp
aCanExp25 seed = Pair (aBinTree seed 24) (aRestGrStr25 seed)

aCanExp50 :: Int -> CanExp
aCanExp50 seed = Pair (aBinTree seed 49) (aRestGrStr50 seed)

aCanExp100 :: Int -> CanExp
aCanExp100 seed = Pair (aBinTree seed 99) (aRestGrStr100 seed)

aCanExp500 :: Int -> CanExp
aCanExp500 seed = Pair (aBinTree seed 499) (aRestGrStr500 seed)

aCanExp1000 :: Int -> CanExp
aCanExp1000 seed = Pair (aBinTree seed 999) (aRestGrStr1000 seed)

-- a random canonical expression, 1st Int is the seed, 2nd Int is the size
-- sz is the number of external leaves and sz-1 is the number of internal nodes
aCanExp :: Int -> Int ->  CanExp
aCanExp seed sz =
  if sz == 10 then aCanExp10 seed
  else if sz == 25 then aCanExp25 seed
  else if sz == 50 then aCanExp50 seed
  else if sz == 100 then aCanExp100 seed
  else if sz == 500 then aCanExp500 seed
  else if sz == 1000 then aCanExp1000 seed
  else Pair (aBinTree seed (sz-1)) (aRestGrStr seed sz)
-- if not 10, 25, 50, 100, 500, 1000 then, in practice,  it is limited to 12

-- ========== Statistics ==========
-- The goal
goal :: LabBinTree -> Int
goal (Leaf n) = n
goal (LNode _ lbt) = goal lbt

-- ~~~~ Silly intuitionistic theorems (see Genitrini, Kozik, Zaionc, def 1) ~~~~

-- test that a canonical expression has the form e1 → ... → xn → ... → xn
-- on labeled binary trees
isSimple  :: LabBinTree -> Bool
isSimple (Leaf _) = False
isSimple (LNode (Leaf n) lbt2) = goal lbt2 == n || isSimple lbt2
isSimple (LNode _ lbt2) = isSimple lbt2

-- test that a canonical expression has the form e1 → ... → x0 → ... → x0
isSimple0 :: LabBinTree -> Bool
isSimple0 (Leaf n) = False
isSimple0 (LNode (Leaf n) lbt2) = n == 0 || isSimple lbt2
isSimple0 (LNode _ lbt2) = isSimple0 lbt2

-- Recursively remove premises that are simple
removeSimplePremises :: LabBinTree -> LabBinTree
removeSimplePremises (Leaf n) = Leaf n
removeSimplePremises (LNode lbt1 lbt2) =
  let lbt1' = removeSimplePremises lbt1
      lbt2' = removeSimplePremises lbt2
  in if isSimple lbt1' then lbt2'
     else LNode lbt1' lbt2'

-- More simple
isSimpleAfterRemoving :: LabBinTree -> Bool
isSimpleAfterRemoving = isSimple . removeSimplePremises

-- ~~~~ Silly intuitionistic theorems ~~~~
premsOf :: LabBinTree -> [LabBinTree]
premsOf (Leaf _) = []
premsOf (LNode e1 e2) = e1 : premsOf e2

(=¤=) :: LabBinTree -> LabBinTree -> Bool
(=¤=) (Leaf n) (Leaf n') = n == n'
(=¤=) (LNode n1 n2) (LNode n1' n2') = n1 =¤= n1' && n2 =¤= n2'
(=¤=) _ _ = False

endsWith :: LabBinTree -> LabBinTree -> Bool
endsWith e@(LNode _ e1) e' = e =¤= e' || e1 `endsWith` e'
endsWith _ _ = False

existsEndsWith :: LabBinTree -> [LabBinTree] -> Bool
existsEndsWith _ [] = False
existsEndsWith lbt (lbt' : l)  = lbt `endsWith` lbt' || existsEndsWith lbt l

--  check for pattern of the form ... → p → ... → p
isSilly :: LabBinTree -> Bool
isSilly lbt = existsEndsWith lbt (premsOf lbt)

-- ~~~~ implicative intuitionistic theorems by modus ponens
-- Check whether an expression has a variabe as premise
-- and returns the first one
hasVarAsPrem :: LabBinTree -> (Bool,Int)
hasVarAsPrem (Leaf _) = (False,0)
hasVarAsPrem (LNode (Leaf i) _) = (True,i)
hasVarAsPrem (LNode (LNode _ _) lbt) = hasVarAsPrem lbt

-- Given i and n, look whether an expression has premise of the form xi → xn
hasImpAsPrem :: Int -> Int -> LabBinTree -> Bool
hasImpAsPrem i n (LNode (LNode (Leaf i') (Leaf n')) lbt) =
  (i == i' && n == n') || hasImpAsPrem i n lbt
hasImpAsPrem i n (LNode _ lbt) = hasImpAsPrem i n lbt
hasImpAsPrem _ _ (Leaf _) = False

-- Check pattern of the form .. → xi → .. (xi → xn) .. → xn
-- or .. (xi → xn) → .. xi → .. → xn
isElim :: LabBinTree -> Bool
isElim lbt =
 let (b,i) = hasVarAsPrem lbt
 in if b then hasImpAsPrem i (goal lbt) lbt else False

isEasy :: LabBinTree -> Bool
isEasy lbt = isSimple lbt || isElim lbt

-- Remove easy premises
-- i.e., simple or Elim
removeEasyPremises :: LabBinTree -> LabBinTree
removeEasyPremises (Leaf n) = Leaf n
removeEasyPremises (LNode lbt1 lbt2) =
  let lbt1' = removeEasyPremises lbt1
      lbt2' = removeEasyPremises lbt2
  in if isEasy lbt1' then lbt2'
     else LNode lbt1' lbt2'

-- Cheap
isCheap :: LabBinTree -> Bool
isCheap lbt = let afterRem = removeEasyPremises lbt
              in isEasy afterRem || isSilly afterRem

-- Remove premises that are easy or silly (perhaps not efficient)
removeEorSPremises :: LabBinTree -> LabBinTree
removeEorSPremises (Leaf n) = Leaf n
removeEorSPremises (LNode lbt1 lbt2) =
  let lbt1' = removeEorSPremises lbt1
      lbt2' = removeEorSPremises lbt2
  in if isEasy lbt1' || isSilly lbt1' then lbt2'
     else LNode lbt1' lbt2'

-- ~~~~ Classical theorems ~~~~
-- count leaves
countL :: BinTree -> Int
countL Empty = 1
countL (Node t1 t2) = countL t1 + countL t2

-- checks whether a canonical expression satisfies an assignment
-- in the [Int] assignmnt values are 0 (False) or 1 (True)
check :: CanExp -> [Int] -> Bool
check (Pair Empty (i : l)) assg = (assg !! i) == 1
check (Pair Empty []) _ = False
check (Pair (Node t1 t2) l) assg =
 not (check (Pair t1 l) assg) || (check (Pair t2 (drop (countL t1) l)) assg)

-- inefficient (cf SAT)
isTautology :: CanExp -> Bool
isTautology e@(Pair t l) = all (check e) (strings (length l) 1)

tautologies :: [[CanExp]]
tautologies = map (filter isTautology) canonicalExps

-- we may assume x0 receives 0 (this saves half of the computation)
-- since if x0 recieves 1 then the valuation of the expression is necessarly 1 (True) 
isTautology' :: CanExp -> Bool
isTautology' e@(Pair t l) = all (check e) (map ((:) 0) (strings ((length l) -1) 1))

tautologies' :: [[CanExp]]
tautologies' = map (filter isTautology') canonicalExps

-- checks whether a LabIntTree expression satisfies an assignment
-- in the [Int] assignment values are 0 (False) or 1 (True)
checkL :: LabBinTree -> [Int] -> Bool
checkL (Leaf n) assg = (assg !! n) == 1
checkL (LNode lbt1 lbt2) assg = not (checkL lbt1 assg) || (checkL lbt2 assg)

-- the maximum variable
maxVar :: LabBinTree -> Int
maxVar (Leaf n) = n
maxVar (LNode lbt1 lbt2) = max (maxVar lbt1) (maxVar lbt2)

-- we may assume α0 receives 0
-- isTautL is supposed to be applied on candidates (selected by not.trivNonClass)
-- after removing trivial premises (see ratioIntVsMyT)
isTautL :: LabBinTree -> Bool
isTautL lbt =
  let maxV = maxVar lbt
  in all (checkL lbt) (map ((:) 0) (strings maxV 1))

-- `trivNonClass` is a  way to eliminate non classical propositions,
-- I assume that it is applied to the LabBinTree image
-- of a canonical expressions (hence goal lbt == 0 i.e., α0 == 0)
-- Assume now α0 := False and αi := True (for i /= 0)
--       and lbti := ... →α0→ ... →lbti' (then lbti := True)
-- or goal lbti /= 0 
--       then lbti := True
-- then ..→ lbti → α0 := False
-- then ..→ lbti → α0 is not a tautology
-- Therefore if trivNonClass(lbt) then lbt is not a tautology.
zeroIsPrem :: LabBinTree -> Bool
zeroIsPrem (Leaf _) = False
zeroIsPrem (LNode (Leaf 0) _) = True
zeroIsPrem (LNode _ lbt) = zeroIsPrem lbt

trivNonClass :: LabBinTree -> Bool
trivNonClass (Leaf _) = True -- a variable is not a classical proposition
trivNonClass (LNode lbt1 lbt2) = (goal lbt1 /= 0 || zeroIsPrem lbt1) && trivNonClass lbt2

-- The definition of Genitrini et al. `simple non tautology` :
-- goals of premisses are not x0
isSimpNonTaut :: LabBinTree -> Bool
isSimpNonTaut (Leaf n) = n == 0
isSimpNonTaut (LNode lbt1 lbt2) = (goal lbt1 /= 0) && isSimpNonTaut lbt2

-- a LabBinTree isWeakTaut if by renaming his indices larger than maxVar to maxVar it isTautL
-- those terms are potential tautologies
-- moreover if an expression is not WeakTaut, then it is not a tautology
isWeakTaut :: Int -> LabBinTree -> Bool
isWeakTaut maxVar lbt =
  let renameBig (Leaf n) = if n < (maxVar - 1) then Leaf n else Leaf (maxVar -1)
      renameBig (LNode lbt1 lbt2) = LNode (renameBig lbt1) (renameBig lbt2)
  in isTautL (renameBig lbt)

-- ~~~~ Statistics ~~~~
-- given the largest variable index, a seed, the size of CanExps, and the size of the sample,
-- returns 6 values
-- the number of simple intuitionistic propositions
--                      after removing trivial premises
-- the number of tautologies that are not of the above family
-- the ratio intuitionist / tautologies
-- the sets of those true tautologies
-- the sets of expressions with too large indices
-- the numbers of expressions that have too large indices
zaionc :: Int -> Int -> Int -> Int ->
                 (Int,Int,Double,[LabBinTree],[LabBinTree],Int)
zaionc limitMaxVar seed sz szS =
    let sample = [canExp2LBT$aCanExp sid sz | sid<-map ((*) sz) [seed..(seed+(szS - 1))]]
        (eOrS,notEOrS) =  partition isCheap sample
        nbEOrS = length eOrS
        candidateTauts = filter (not.trivNonClass) notEOrS
        (small, big) = partition (\e -> (maxVar e) < limitMaxVar) candidateTauts
        classTauts = filter isTautL small
        nbClassTauts = length classTauts {-- nb of true classical tautologies --}
        bigs = filter (isWeakTaut limitMaxVar) big {-- big expressions that are potential tautologies --}
        tooBig = length bigs
    in (nbEOrS, nbClassTauts,
        (fromIntegral nbEOrS) / (fromIntegral (nbEOrS + nbClassTauts)),
        classTauts,bigs,tooBig)

-- given the size of CanExp, and the size of the sample, and a seed
-- returns  tautologies which are not an easy or stupid theorem
someTaut :: Int -> Int -> Int -> Int -> [LabBinTree]
someTaut seed sz szS limitMaxVar =
  let sample = [canExp2LBT$aCanExp sid sz | sid <- [seed..(seed+szS-1)]]
  in filter isTautL $ filter (\e -> (maxVar e) < limitMaxVar) $ filter (not.trivNonClass) $ filter (not.isCheap) $ map removeEasyPremises sample

generateAndCount :: Int -> Int -> (Int -> a) -> (a -> Bool) -> Int
generateAndCount seed max aRand p = if max == 0 then 0
                                    else let n = generateAndCount (seed + 1) (max - 1) aRand p
                                         in if p (aRand seed) then n + 1 else n
-- To check Genitrini et al. method, ratio simple over non simple tautologies
genitrini :: Int -> Int -> Int -> (Int,Int)
genitrini seed sz szS =
    let (simp,notSimp) =  partition isSimple [canExp2LBT$aCanExp sid sz | sid<-map ((*) sz) [seed..(seed+(szS - 1))]]
        nbSimp = length simp
        nbNonSimpNonTaut = length $ filter (not.isSimpNonTaut) notSimp
    in (nbSimp,nbNonSimpNonTaut)
