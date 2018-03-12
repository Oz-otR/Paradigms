import Data.Char
import Data.List

main = putStrLn . show $ parseNode testlist currentPath testTNP 0

testlist = [[0,2,-1],
            [-1,1,0],
            [1,0,-1]]

testTNP =   [[0,0,-1],
             [0,0,9],
             [0,0,0]]

currentPath = [0,0,0]
bestPath = [[0,0,0],[0]]


-- Finds biggest number in a 2-D list
parseNode :: [[Int]] -> [[Int]] -> [Int] -> Int -> [Int]
parseNode list tnplist currentPath totalPen
    | taillist /= [] =
        myMin [parseNode (safeAdd2DArr (editCols (-1) index taillist) (tnplist !! index)) tnplist (currentPathSet index) (totalPen + (taskPenalty index)) | index <- [0..2], (taskPenalty index) /= -1]
    | otherwise = parseLeaf headlist totalPen
    where   newTaillist n = safeAdd2DArr taillist n  --adds TNP to head of tail list
            taskPenalty n = (headlist !! n)
            headlist:taillist = list
            currentPathSet n m = currentPath !!  --Could choose   "do (currentPath !! length m) = n    *takes index list

--Or, pass [[0,0,0],[0,0,0],[0]] down the recursion chain and parse them on the way up

parseLeaf :: [Int] -> [Int] -> Int -> Int
parseLeaf list currentPath totalPen  = [parseNode currentPath (totalPen + (taskPenalty index)) | index <- [0..2], (taskPenalty index) /= -1] --penalty stuff
    where   taskPenalty n = (list !! n)


myMin $ map (+totalPen) $ filter (/= -1) list 
    where headlist:remaininglist = list



--Takes a 2D list and adds a 1D list to it
safeAdd2DArr :: [[Int]] -> [Int] -> [[Int]]
safeAdd2DArr penlist tnplist = (safeAdd1DArr x tnplist):xs
    where x:xs = penlist

safeAdd1DArr :: [Int] -> [Int] -> [Int]
safeAdd1DArr = zipWith $ \a b -> if a == -1 || b == -1  then -1 else a + b


editCol :: Int -> Int -> [Int] -> [Int]
editCol n 0 (x:xs) = n:xs
editCol n i (x:xs) = x:editCol n (i - 1) xs

editCols :: Int -> Int -> [[Int]] -> [[Int]]
editCols n i = map (editCol n i)

myMin [] = maxBound :: Int
myMin xs = minimum xs  

