main = putStrLn . show $ parseNode testArray 0 0 0

testArray = [[4,2,1],[-1,3,7],[2,-1,8]]
testTNP = [[0,1,1],[1,0,1],[1,1,0]]
--testArray = [4,2,0]

-- Finds biggest number in a 2-D array
parseNode :: (Num a, Ord a) => [[a]] -> a -> a -> a -> a
parseNode array totalPen curTaskInd lastTaskInd
    | remainingArray /= [] =
        minimum [parseNode remainingArray (totalPen+penalty) curTaskInd lastTaskInd | penalty <- headArray, penalty /= -1]
    | otherwise = parseLeaf headArray totalPen curTaskInd lastTaskInd
    where   headArray:remainingArray = array
            tnp = (testTNP !! curTaskInd) !! lastTaskInd


parseLeaf :: (Num a, Ord a) => [a] -> a -> a -> a -> a
parseLeaf array totalPen curTaskInd lastTaskInd = minimum (map (+totalPen) (filter (/= -1) array))
    where headArray:remainingArray = array






{-


main = putStrLn . show $ parseNode testArray 0 0 0

testArray = [[4,2,1],[-1,3,7],[2,-1,8]]
testTNP = [[0,1,1],[1,0,1],[1,1,0]]
--testArray = [4,2,0]

-- Finds biggest number in a 2-D array
parseNode :: (Num a, Ord a) => [[a]] -> a -> a -> a -> a
parseNode array totalPen curTaskInd lastTaskInd
    | remainingArray /= [] = minimum 
        [parseNode remainingArray (totalPen+penalty) curTaskInd lastTaskInd | penalty <- headArray, penalty /= -1]
    | otherwise = parseLeaf headArray totalPen curTaskInd lastTaskInd
    where headArray:remainingArray = array


parseLeaf :: (Num a, Ord a) => [a] -> a -> a -> a -> a
parseLeaf array totalPen curTaskInd lastTaskInd = minimum (map (+totalPen) (filter (/= -1) array))
    where headArray:remainingArray = array


















-- Finds biggest number in a 2-D array
parseNode :: (Num a, Ord a) => [[a]] -> a -> a
parseNode array totalPen
    | array != [] = [parseNode remainingArray penalty | penalty <- headArray]
    | otherwise = [parseLeaf remainingArray (totalPen+penalty) | penalty <- headArray]
        where headArray:remainingArray = array

parseLeaf :: (Num a, Ord a) => [a] -> a -> a
parseLeaf array totalPen = 3
    where headArray:remainingArray = array








-- Finds biggest number in a 2-D array
parse :: (Num a, Ord a) => [[a]] -> a -> a
parse array totalPen =
    | null == array = [parseNode xs (totalPen+x) | x <- array]
    | otherwise = [parseLeaf xs (totalPen+x) | x <- array]
        where x:xs = array

parse :: (Num a, Ord a) => [a] -> a
parse array = maximum x
    where x:xs = array














-- Finds biggest number in a 2-D array   (Works)
parse :: (Num a, Ord a) => [[a]] -> a
parse array = maximum [maximum x | x <- array]




--parse array depth biggest = foldl (\acc x -> if x > acc then acc = x else acc) biggest 



parse :: (Num a) => [[a]] -> a -> a -> a
parse array depth biggest = sum (map sum array)





main = (putStrLn . show . parse) testArray

testArray = [[4,2,0],[0,7,3],[-1,3,-1]]

parse :: (Num a) => [[a]] -> a
parse array = sum (map sum array)
-}
