import Data.Char
import Data.List

{-Logic now works for 8x8 currenttly-}

main = putStrLn . show $ (parseNode testlist testTNP currentPath 0)  --Prints lowest penalty of search

testlist = [[-1,5,5,-1,-1,-1,-1,-1],
            [-1,6,6,-1,-1,-1,-1,-1],
            [-1,-1,-1,7,-1,-1,-1,-1],
            [-1,-1,-1,-1,8,-1,-1,-1],
            [-1,-1,-1,-1,-1,9,-1,-1],
            [-1,-1,-1,-1,-1,-1,10,-1],
            [-1,-1,-1,-1,-1,-1,-1,11],
            [12,-1,-1,-1,-1,-1,-1,-1]]


testTNP =  [[0,0,0,0,0,0,0,0],
            [0,0,5,0,0,0,0,0],
            [0,0,0,6,0,0,0,0],
            [0,0,0,0,0,0,0,0],
            [0,0,0,0,0,0,0,0],
            [0,0,0,0,0,0,0,0],
            [0,0,0,0,0,0,0,0],
            [0,0,0,0,0,0,0,0]]

currentPath = []
worstPath = [[0,0,0,0,0,0,0,0],[maxBound :: Int]]


-- Finds biggest number in a 2-D list
parseNode :: [[Int]] -> [[Int]] -> [Int] -> Int -> [[Int]]
parseNode list tnplist currentPath totalPen
    | taillist /= [] =
        returnLowPen [parseNode (safeAdd2DLst (editCols (-1) index taillist) (tnplist !! index)) tnplist  (addToPath index) (totalPen + (headlist !! index)) | index <- [0..7], (headlist !! index) /= -1]
    | otherwise = returnLowPen [assembleLeaf (addToPath index) (addFirstTNP index) | index <- [0..7], (headlist !! index) /= -1]
    where   newTaillist n = safeAdd2DLst taillist n  --adds TNP to head of tail list
            headlist:taillist = list
            addToPath n = n:currentPath
            addFirstTNP n = (totalPen + (safeAdd1DLst headlist (tnplist !! (currentPath !! 0))) !! n)


--Assembles the leaf information into [[path][pen]]
assembleLeaf :: [Int] -> Int -> [[Int]]
assembleLeaf currentPath totalPen  = ((reverse currentPath):[[totalPen]])


returnLowPen :: [[[Int]]] -> [[Int]]
returnLowPen list = foldl(\acc leaf -> if (acc !! 1) < (leaf !! 1) then acc else leaf) worstPath list   --acc is a single 2D array leaf information packet [[path][pen]]


--Takes a 2D list and adds a 1D list to it except for -1s
safeAdd2DLst :: [[Int]] -> [Int] -> [[Int]]
safeAdd2DLst penlist tnplist = (safeAdd1DLst x tnplist):xs
    where x:xs = penlist


safeAdd1DLst :: [Int] -> [Int] -> [Int]
safeAdd1DLst = zipWith $ \a b -> if a == -1 || b == -1  then -1 else a + b


--Changes the value for an entire column of a 2d array to selected value
editCol :: Int -> Int -> [Int] -> [Int]
editCol n 0 (x:xs) = n:xs
editCol n i (x:xs) = x:editCol n (i - 1) xs

editCols :: Int -> Int -> [[Int]] -> [[Int]]
editCols n i = map (editCol n i)
