module GeniusSquare where

--import Data.Matrix
import Data.Matrix as M
import Data.Char
import Data.List
import Data.Either
--import Data.SBV
import Data.Maybe
--import Data.SBV.Internals
--import System.TimeIt


{-
POLYOMINO SECTION
-}

p1, p2i1, p2i2, p3i1, p3i2, p4i1, p4i2, p4i3, p4i4, p5i1, p5i2, p6, p7i1, p7i2, p7i3, p7i4, p8i1, p8i2, p8i3, p8i4, p8i5, p8i6, p8i7, p8i8, p9i1, p9i2, p9i3, p9i4 :: Num a => Matrix a
p1all, p2all, p3all, p4all, p5all, p6all, p7all, p8all, p9all :: Num a => [Matrix a]
--P1: MONOMINO
p1 = fromList 1 1 [1]
p1all = [p1]

--P2: DOMINO
p2i1 = fromList 1 2 [1,1]
p2i2 = M.transpose p2i1
p2all = [p2i1,p2i2]

--P3: STRAIGHT TROMINO
p3i1 = fromList 1 3 [1,1,1]
p3i2 = M.transpose p3i1
p3all = [p3i1,p3i2]

--P4: RIGHT TROMINO
p4i1 = fromLists [[1,1],[1,0]]
p4i2 = switchCols 1 2 p4i1
p4i3 = switchRows 1 2 p4i2
p4i4 = switchCols 1 2 p4i3
p4all = [p4i1,p4i2,p4i3,p4i4]

--P5: STRAIGHT TETROMINO
p5i1 = fromList 1 4 [1,1,1,1]
p5i2 = M.transpose p5i1
p5all = [p5i1,p5i2]

--P6: SQUARE TETROMINO
p6 = fromLists [[1,1],[1,1]]

p6all = [p6]

--P7: T TETROMINO
p7i1 = fromLists [[0,1,0],[1,1,1]]
p7i2 = M.transpose p7i1
p7i3 = switchRows 1 2 p7i1
p7i4 = M.transpose p7i3

p7all = [p7i1,p7i2,p7i3,p7i4]

--P8: L TETROMINO
p8i1 = fromLists [[1,1,1],[1,0,0]]
p8i2 = switchCols 1 3 p8i1
p8i3 = switchRows 1 2 p8i1
p8i4 = switchCols 1 3 p8i3
p8i5 = M.transpose p8i1
p8i6 = switchRows 1 3 p8i5
p8i7 = switchCols 1 2 p8i5
p8i8 = switchRows 1 3 p8i7

p8all = [p8i1,p8i2,p8i3,p8i4,p8i5,p8i6,p8i7,p8i8]

--P9: SKEW TETROMINO
p9i1 = fromLists [[0,1,1],[1,1,0]]
p9i2 = switchRows 1 2 p9i1
p9i3 = M.transpose p9i1
p9i4 = M.transpose p9i2

p9all = [p9i1,p9i2,p9i3,p9i4]

unusedPi :: Num a => [[Matrix a]]
unusedPi = [p1all,p2all,p3all,p4all,p5all,p6all,p7all,p8all,p9all]

{-
DICE SECTION
-}

die1 = [(4,5),(5,4),(5,5),(5,6),(6,4),(6,5)]
die2 = [(1,2),(1,3),(2,1),(2,2),(2,3),(3,2)]
die3 = [(1,1),(3,1),(4,1),(4,2),(5,2),(6,3)]
die4 = [(1,6),(6,1)]
die5 = [(2,4),(3,3),(3,4),(4,3),(4,4),(5,3)]
die6 = [(1,4),(2,5),(3,5),(3,6),(4,6),(6,6)]
die7 = [(1,5),(2,6),(5,1),(6,2)]

allDice = [die1,die2,die3,die4,die5,die6,die7]
allPos = concat[die1,die2,die3,die4,die5,die6,die7]

allBoards allDice = [matrix 6 6 $ \(i,j) -> if (i,j) `elem` [d1,d2,d3,d4,d5,d6,d7] then 1 else 0| d1 <- die1, d2 <- die2, d3 <- die3, d4 <- die4, d5 <- die5, d6 <- die6, d7 <- die7]

--A matrix showing the different dice zones
zoneBoard :: Num a => [[(Int,Int)]] -> Matrix a
zoneBoard allDice = matrix 6 6 $ \(i,j) -> f (i,j)
    where
        f (i,j) | elem (i,j) die1 = 1
                | elem (i,j) die2 = 2
                | elem (i,j) die3 = 3
                | elem (i,j) die4 = 4
                | elem (i,j) die5 = 5
                | elem (i,j) die6 = 6
                | elem (i,j) die7 = 7
                | otherwise = 0

howManyBoards :: [[(Int,Int)]] -> Int
howManyBoards allDice = length(allBoards allDice)

{-
MODEL SETUP
-}

ins :: Num a => Matrix a -> Matrix a -> Int -> Int -> Matrix a
ins rMat pMat 0 0 = extendTo 0 (nrows pMat) (ncols rMat) pMat
ins rMat pMat d 0 = (zero (nrows rMat) d) <|> (extendTo 0 (nrows rMat) (ncols rMat - d) pMat)
ins rMat pMat 0 e = (zero e (ncols rMat)) <-> (extendTo 0 (nrows pMat) (ncols rMat) pMat)
ins rMat pMat d e = (zero e (ncols rMat)) <-> ((zero (nrows rMat - e) d) <|> (extendTo 0 (nrows rMat - e) (ncols rMat - d) pMat))

sCheck :: (Num a, Eq a) => Matrix a -> Matrix a -> Int
sCheck rMat pMat = length(filter (==1) ([if (elem 2 (zipWith (+) rlist (toList exp))) then 0 else 1 | exp <- [ins rMat pMat d e| d <- [0..(abs((ncols rMat) - (ncols pMat)))], e <- [0..(abs((nrows rMat) - (nrows pMat)))]]])) 
    where
        rlist = toList rMat

sCalculate :: (Num a, Eq a) => Matrix a -> [Matrix a] -> Int
sCalculate rMat pMatList = sum([sCheck rMat x | x <- pMatList])

potAGen :: (Num a, Eq a) => Matrix a -> Matrix a -> [Matrix a]
potAGen rMat pMat = [elementwise (+) rMat (extendTo 0 6 6 exp)| exp <- [ins rMat pMat d e| e <- [0..(abs((nrows rMat) - (nrows pMat)))], d <- [0..(abs((ncols rMat) - (ncols pMat)))]]]

setnotcontains :: (Num a, Eq a) => a -> Matrix a -> Bool
setnotcontains a potA = if elem a potA then False else True

seriesi :: (Num a, Eq a) => [Matrix a] -> Matrix a -> [Matrix a]
seriesi piall rMat = [elementwise (-) p rMat | p <- (filter (setnotcontains 2) (concat[potAGen rMat pMat | pMat <- piall]))]

sListZip :: (Num a, Eq a) => Matrix a -> [[Matrix a]] -> [(Integer, [Matrix a])]
sListZip rMat piall = zip [1..] [seriesi p rMat | p <- piall]

mColumns :: (Num a, Eq a) =>  Matrix a -> [(Integer, [Matrix a])] -> [[a]]
mColumns rMat pTuples = concat[someMColumns rMat pTup | pTup <- pTuples]


mToMat :: (Num a, Eq a) => Matrix a -> [[Matrix a]] -> Matrix a
mToMat rMat piall = M.transpose(fromLists (mColumns rMat (sListZip rMat piall)) )

someMColumns :: (Num a, Eq a) => Matrix a -> (Integer, [Matrix a]) -> [[a]]
someMColumns rMat (p, as) = [(blockRemAij rMat aMat) ++ (lastBit (fromIntegral p)) | aMat <- as] -- instead of (toList aMat) can use (blockRemAij rMat aMat) to remove blockers
    where
        lastBit p = (toLists (identity 9)) !! (p-1)

{-
DIRECT SOLUTION
-}

--Solve a matrix (must already be in REF form) by back substitution.
substitute :: (Num a) => [[a]] -> [a]
substitute matrix = foldr next [last (last matrix)] (init matrix) where
 
 next row found = let
  subpart = init $ drop (length matrix - length found) row
  solution = last row - sum (zipWith (*) found subpart)
  in solution : found


setNotOnlyZero :: (Num a, Eq a, Fractional a) => [a] -> Bool
setNotOnlyZero l = if any (/= 0.0) l then True else False

rank :: (Num a, Eq a, Fractional a) => Matrix a -> Int --IN RREF
rank mat = length(filter (setNotOnlyZero) [l | l <- toLists mat])

getr :: (Fractional a, Eq a) => Matrix a -> Int -> Int
getr m x
    | getElem x x m /= 1.0 = x
    | otherwise = getr m (x+1)

toUppTri :: (Num a, Eq a, Fractional a) => Matrix a -> [Int] -> (Matrix a, [Int])
toUppTri mat lst
    | ncols mat == nrows mat = (mat, lst)
    | otherwise = toUppTri (fromLists ((take ((getr mat 1) - 1) (ms mat)) ++ (ms (nextRow (getr mat 1))) ++ (drop ((getr mat 1) - 1) (ms mat)))) (lst ++ [((getr mat 1) - 1)])  --toUppTri(mat <-> nextRow 3)
    where
        nextRow r = matrix 1 (ncols mat) $ \(i,j) -> if (i,j) == (1,r) then 1 else 0
        ms mat = toLists mat

comp :: (Num a, Eq a, Fractional a) => [Matrix a] -> [Int]
comp boards = [rank (myFunc (rref ((mToMat b unusedPi) <|> ( matrix 38 1 $ \(i,j) -> 1)))) | b <- boards]

bigM :: (Num a, Eq a) => Matrix a -> [[Matrix a]] -> Matrix a
bigM rMat piall = ((mToMat rMat piall) <|> ( matrix 38 1 $ \(i,j) -> 1))

blockRemAij :: (Num a, Eq a) => Matrix a -> Matrix a -> [a]
blockRemAij rMat aMat = filter (/= 2) [new (toList rMat) (toList aMat) i | i <- [0..35]]
    where
        new rs as i = if ((rs !! i) == 0) then (as !! i) else 2

myFunc :: Either String a -> a
myFunc (Right number) = number
myFunc (Left _)       = error "There is no number"

{-
{-
LINEAR OPTIMISER SOLUTION
-}

solveIntegerLinearEqsAll :: Solver          -- ^ SMT Solver to use
                          -> Int                -- ^ Maximum number of solutions to return, in case infinite
                         -> [[Integer]]     -- ^ Coefficient matrix (A)
                         -> [Integer]       -- ^ Result vector (b)
                         -> IO [[Integer]]  -- ^ All solutions to @Ax = b@
solveIntegerLinearEqsAll s maxNo coeffs res = extractModels `fmap` allSatWith cfg cs
  where cs = buildConstraints "solveIntegerLinearEqsAll" coeffs res
        cfg = (defaultSolverConfig s) {allSatMaxModelCount = Just maxNo}


-- | Build the constraints as given by the coefficient matrix and the resulting vector
buildConstraints :: (Ord a, Num a, SymVal a) => String -> [[a]] -> [a] -> Symbolic SBool
buildConstraints f coeffs res
  | m == 0 || any (/= n) ns || m /= length res
  = error $ f ++ ": received ill-formed input."
  | True
  = do xs <- Data.SBV.mkFreeVars n
       let rowEq row r = sum (zipWith (*) xs row) .== r
       solve $ zipWith rowEq (map (map literal) coeffs) (map literal res)
 where m    = length coeffs
       n:ns = map length coeffs

solveIt mat vect = do
  candidates <- solveIntegerLinearEqsAll Z3 1500 mat vect
  print(head (filter listBinary candidates))

solveIt' n mat vect = do
  candidates <- solveIntegerLinearEqsAll Z3 n mat vect
  pure (filter listBinary candidates)
-}
listBinary :: [Integer] -> Bool
listBinary xs = all (\x -> x == 0 || x == 1) xs


{-
LABELLED MATRIX SOLUTION
-}

labelSeries :: (Num a, Eq a) => Matrix a -> [[Matrix a]] -> [[Matrix a]]
labelSeries rMat piall = [[elementwise (*) s (matrix 6 6 $ \(i,j) -> (fromIntegral pn))| s <- (seriesi (piall !! (pn - 1)) rMat)] | pn <- [1..9]]

together :: (Num a, Eq a) => Matrix a -> [[Matrix a]] -> [[Matrix a]]
together rMat unusedPi = recTog 0 labelled
  where
    labelled = labelSeries rMat unusedPi

recTog :: (Num a, Eq a) => Int -> [[Matrix a]] -> [[Matrix a]]
recTog 8 sets = sets
recTog h sets = recTog (h+1) ((filter (snc h) [elementwise (+) s1 s2 | s1 <- sets !! 0, s2 <- sets !! 1]): (drop 2 sets))

    
snc :: (Num a, Eq a) => Int -> Matrix a -> Bool
snc h mat = (all (/= fromIntegral(h+3)) (concat(toLists mat)))
        && (all (/= fromIntegral(h+4)) (concat(toLists mat)))
        && (all (/= fromIntegral(h+5)) (concat(toLists mat)))
        && (all (/= fromIntegral(h+6)) (concat(toLists mat)))
        && (all (/= fromIntegral(h+7)) (concat(toLists mat)))
        && (all (/= fromIntegral(h+8)) (concat(toLists mat)))
        && (all (/= fromIntegral(h+9)) (concat(toLists mat)))
        && (all (/= fromIntegral(h+10)) (concat(toLists mat))) 


{-
HINT DEVELOPMENT (IN PROGRESS)
-}

placePlaced :: a -> Int -> Matrix a -> IO(Matrix a)
placePlaced placed 0 mat = return mat
placePlaced placed x mat = do

    putStrLn "Which position in the above matrix is it occupied by?"
    pos1 <- getLine
    let pos2 = read pos1
    let temp = setElem placed ((fst (divMod pos2 6)) + 1, snd (divMod pos2 6)) mat
    placePlaced placed (x-1) temp

{-
CURRENT USER INTERFACE
-}

showBoard :: String
showBoard  = "\n |a|b|c|d|e|f|\n--------------\n1| | | | | | |\n--------------\n2| | | | | | |\n--------------\n3| | | | | | |\n--------------\n4| | | | | | |\n--------------\n5| | | | | | |\n--------------\n6| | | | | | |\n--------------\n"

pickChoice :: (Num a) => IO(Matrix a)
pickChoice = do
    putStrLn showBoard
    putStrLn "Pick your choice for the first die:\n1. (4,e)\n2. (5,d)\n3. (5,e)\n4. (5,f)\n5. (6,d)\n6. (6,e)"
    dc1 <- getLine
    putStrLn "Pick your choice for the second die:\n1. (1,b)\n2. (1,c)\n3. (2,a)\n4. (2,b)\n5. (2,c)\n6. (3,b)"
    dc2 <- getLine
    putStrLn "Pick your choice for the third die:\n1. (1,a)\n2. (3,a)\n3. (4,a)\n4. (4,b)\n5. (5,b)\n6. (6,c)"
    dc3 <- getLine
    putStrLn "Pick your choice for the fourth die:\n1. (1,f)\n2. (6,a)"
    dc4 <- getLine
    putStrLn "Pick your choice for the fifth die:\n1. (2,d)\n2. (3,c)\n3. (3,d)\n4. (4,c)\n5. (4,d)\n6. (5,c)"
    dc5 <- getLine
    putStrLn "Pick your choice for the sixth die:\n1. (1,d)\n2. (2,e)\n3. (3,e)\n4. (3,f)\n5. (4,f)\n6. (6,f)"
    dc6 <- getLine
    putStrLn "Pick your choice for the seventh die:\n1. (1,e)\n2. (2,f)\n3. (5,a)\n4. (6,b)"
    dc7 <- getLine
    return (matrix 6 6 $ \(i,j) -> if (i,j) `elem` [(die1 !! (read dc1 - 1)),(die2 !! (read dc2 - 1)),(die3 !! (read dc3 - 1)),(die4 !! (read dc4 - 1)),(die5 !! (read dc5 - 1)),(die6 !! (read dc6 - 1)),(die7 !! (read dc7 - 1))] then 1 else 0)
    --putStrLn "Your board looks like this:"
    --return bn

randomChoice :: (Num a) => IO(Matrix a)
randomChoice = do   
    putStrLn "Pick a number between 1 and 62208"
    boardNumber <- getLine
    return (allBoards allDice !! (read boardNumber - 1))


main = do
    putStrLn "Hello! Here is the general layout of the Genius Square board:"
    putStrLn showBoard
    putStrLn "Would you like to:\n1. Generate a random board\n2. Pick your own coordinates"
    choice <- getLine      
    let bn1 = if choice == "1" then randomChoice else pickChoice
    bn <- bn1
    putStrLn "Your board looks like this:"
    print bn
    putStrLn "The solution boards are labelled as follows:\n0: BLOCKER\n1: MONOMINO\n2: DOMINO\n3: STRAIGHT TROMINO\n4: RIGHT TROMINO\n5: STRAIGHT TETROMIN\n6: SQUARE TETROMINO\n7: T TETROMINO\n8: L TETROMINO\n9: SKEW TETROMINO"
    print $ together bn unusedPi
    print $ length((together bn unusedPi)!!0)


    --print $ together bn unusedPi [1..9]
    --print $ length((together bn unusedPi [1..9])!!0)