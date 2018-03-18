import Data.Char
import Data.List

main = putStrLn . show $ (parseNode testlist testTNP currentPath 0)  --Prints lowest penalty of search

testlist = [[1,1,2],
            [1,1,1],
            [1,1,2]]

testTNP =   [[0,0,0],
             [0,0,0],
             [0,0,0]]

currentPath = []
bestPath = [[0,0,0],[0]] -- Delete this line
worstPath = [[0,1,2],[500]]


-- Finds biggest number in a 2-D list
parseNode :: [[Int]] -> [[Int]] -> [Int] -> Int -> [[Int]]
parseNode list tnplist currentPath totalPen
    | taillist /= [] =
        returnLowPen [parseNode (safeAdd2DLst (editCols (-1) index taillist) (tnplist !! index)) tnplist  (addToPath index) (totalPen + (headlist !! index)) | index <- [0..2], (headlist !! index) /= -1]
    | otherwise = returnLowPen [assembleLeaf (addToPath index) (addFirstTNP index) | index <- [0..2], (headlist !! index) /= -1]
    where   newTaillist n = safeAdd2DLst taillist n  --adds TNP to head of tail list
            headlist:taillist = list
            addToPath n = n:currentPath
            addFirstTNP n = (totalPen + (safeAdd1DLst headlist (tnplist !! (currentPath !! 0))) !! n)


--Assembles the leaf information into [[path][pen]]
assembleLeaf :: [Int] -> Int -> [[Int]]
assembleLeaf currentPath totalPen  = (currentPath:[[totalPen]])


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
