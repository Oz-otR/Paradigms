import System.Environment
import System.Exit
import System.IO
import Control.Exception
import Data.IORef
import Data.List
import Data.Maybe
import Data.Char
import Text.Read


tnList  =   [[0,0,0,0,0,0,0,0],
            [0,0,0,0,0,0,0,0],
            [0,0,0,0,0,0,0,0],
            [0,0,0,0,0,0,0,0],
            [0,0,0,0,0,0,0,0],
            [0,0,0,0,0,0,0,0],
            [0,0,0,0,0,0,0,0],
            [0,0,0,0,0,0,0,0]]


-- section Names?
sections = ["Name:"
            ,"forced partial assignment:"
            ,"forbidden machine:"
            ,"too-near tasks:"
            ,"machine penalties:"
            ,"too-near penalities"
            ,"\n"]
main = turn
{-
declare a catch
gets command line args
if there arent 2 prints wrong number of args
else try to read file
if file doesnt exist print out File not found
else read all contents, and pass on both the outstream & content for parsing
-}
turn :: IO()
turn = handle handler $ do
    args <- getArgs
    if length args /= 2
       then do
            putStrLn $ errPrnt 1
            exitFailure
       else case init args of
              [file] -> do
                x <- try $ readFile file
                case x of
                  Left exc -> handler exc
                  Right content -> pass (args !! 1) content
handler :: IOError -> IO ()
handler ex = putStrLn (errPrnt 0) >> exitFailure
writeout :: FilePath -> Int -> IO ()
writeout path x = writeFile path (errPrnt x)
{-
Check if theres at least one \n between each sections.
Filters out return carriage for in Windows Based Files
prints parse error and exits if false
prunes all empty strings, lines that are just the newline char
get indexes of section header in the content list
if section arent in order print parse eror and exit, Compares Indexes
recursively split eachsection into thier own list for parsing
send the list of sections and the out stream to parsedriver to be parsed
-}
pass :: p -> [Char] -> IO ()
pass out content = do
     { if not(leastone (filter (/= '\r') content) 1)
          then putStrLn (errPrnt 10) >> exitFailure
      else return()
     ; let pruned = removeEmpties $ lines $ filter (/= '\r') content
     ; let index = getIn 0 pruned
     ; if not(sorted index && not ((-1) `elem` index))
          then putStrLn (errPrnt 10) >> exitFailure
      else return ()
     ; let blocks = reverse $ splt 5 pruned index
     ; parseDriver out blocks -- print blocks to view the list
     }
{-
takes the out stream and a list of strings to parse
sends each section off to their parsers
-}
parseDriver :: p -> [[[Char]]] -> IO ()
parseDriver out content = do
     { pname out $ content !! 0
     ; pfpa out $ content !! 1
     ; pfm out $ content !! 2
     ; ptnt out $ content !! 3
     ; pmp out $ content !! 4
     ; ptnp out $ content !! 5
     ; print $ content !! 3
     --; mapM_ print  $map taskToCoord $ map listToTuple $ map words (tupleFuckery (content !! 1)) --("1", "A") turned into (0, 0)
     --; mapM_ print  $map taskToCoord $ map listToTuple $ map words (tupleFuckery (content !! 2)) --("1", "A") turned into (0, 0)
     ; mapM_ print  $map lefttaskToCoord $ map righttaskToCoord $ map listToTuple $ map words (tupleFuckery (content !! 3))
     ; let splitByWords = mapByWord $ content !! 4
     ; let (_:end) = splitByWords
     ; let penList = mapFullToInt end
     ;print penList
 --    ; mapM_ print $ forceAssign penList $map taskToCoord $ map listToTuple $ map words (tupleFuckery (content !! 1))
     ; let forceAppList = forceAssign penList $map taskToCoord $ map listToTuple $ map words (tupleFuckery (content !! 1))
     ; let finPenList = forbidAssign forceAppList $map taskToCoord $ map listToTuple $ map words (tupleFuckery (content !! 2))
    --VALID CHECK NEEDS to have been called with no errors before this
     ; let tntList = forbidAssign tnList $map lefttaskToCoord $ map righttaskToCoord $ map listToTuple $ map words (tupleFuckery (content !! 3))
     ;print $map leftThruppleToCoord $ map rightThruppleToCoord $ map listToThrupple $ map words (tupleFuckery (content !! 5))
     ; let tntPens = changeValue tntList $map leftThruppleToCoord $ map rightThruppleToCoord $ map listToThrupple $ map words (tupleFuckery (content !! 5))
     ; mapM_ print forceAppList
     ; putStrLn "\n"
     ; mapM_ print finPenList
     ; putStrLn "\n"
     ; mapM_ print tntList
     ; print finPenList
     ; putStrLn . show $ (parseNode finPenList tntPens currentPath 0) --Prints lowest penalty of search
     }

mapByWord :: [String] -> [[String]]
mapByWord l = map words l
mapToInt :: [String] -> [Int]
mapToInt l = map read l
mapFullToInt :: [[String]] -> [[Int]]
mapFullToInt l = do
  w <- l
  return $ mapToInt w
{-
checks if theres more than one name or if theres 2 names separated by a ' '
prints parse error and exits if true
-}
pname :: Foldable t => p -> [t Char] -> IO ()
pname out l = if ((length l) /= 2 || ' ' `elem` (l !! 1) )
             then putStrLn (errPrnt 10) >> exitFailure
         else return()
         --writeout out 10 >> exitFailure
{-
If the section only contains the name then return
else send the recursive checker to see if all arguments have the correct
pattern. Uses Flag FF (forced/forbidden)
if one line is in correct Prints invalid machine/task and exits
-}
pfpa :: p -> [[Char]] -> IO ()
pfpa out l = if ((length l) == 1)
            then return ()
            else if rc' l 1 "FF"
                then return()
                else putStrLn (errPrnt 3) >> exitFailure
        --writeout out 3 >> exitFailure
{-
Exactly the Same as Above
-}
pfm :: p -> [[Char]] -> IO ()
pfm out l = if ((length l) == 1)
           then return()
           else if rc' l 1 "FF"
               then return()
               else putStrLn (errPrnt 3) >> exitFailure
           --writeout out 3 >> exitFailure
{-
if section only contains the name return
else recursively check using TNT flag
if true return else print invalid task decsription and exit
-}
ptnt :: p -> [[Char]] -> IO ()
ptnt out l = if ((length l) == 1)
            then return ()
            else if rc' l 1 "TNT"
                then return()
            else putStrLn (errPrnt 5) >> exitFailure
        --writeout out 5 >> exitFailure
{-
uses its out recursive checker, No flag
if checker returns true then return else print the associated error and exit
invalid penalty or machine penalty error
-}
-- check number of occurences of ' '
-- check length $ words line
--
pmp :: p -> [[Char]] -> IO ()
pmp out l = if ((length l) /= 9)
           then putStrLn (errPrnt 4) >> exitFailure
           else if (mprc l 1 == 0)
           then return()
           else putStrLn (errPrnt $ mprc l 1) >> exitFailure
           --writeout out (mprc l 1) >> exitFailure
{-
if section only contains header return
else recursively check using TNP flag
if true return else print invalid task decription and exit
-}
ptnp :: p -> [[Char]] -> IO ()
ptnp out l = if ((length l) == 1)
            then return ()
            else if rc' l 1 "TNP"
                then return()
                else putStrLn (errPrnt 10) >> exitFailure
        --writeout out 5 >> exitFailure
------------------------------------------------------------------------------------
{-
recursive checker
takes a list an idex to start from and a verifier to check agasint
checks if each line in that secion conforms to its pattern
-}
rc' :: [[Char]] -> Int -> [Char] -> Bool
rc' arg index verifier = if (index < (length arg) && ((patt $ arg !! index) == verifier))
                            then rc' arg (index + 1) verifier
                else if index == length arg
                    then True
                else False
{-
recursive checker for machine penalty
cehcks for length and negatives
returns an int to be checked or used as an arguments for errprint
-}
mprc :: Num p => [[Char]] -> Int -> p
mprc args index = if index > 8
                 then 0
             else if (isInfixOf "-" $ args !! index)
                         then 6
                 else if(not((countSpaces $ args !! index) == 7 && (length $ words $ (args !! index)) == 8))
                     then 4
                 else if(rnc 0 $ args !! index)
                     then mprc args (index +1)
                 else 4



-------------------------------------------------------------------------------------------
rnc :: Int -> String -> Bool
rnc index list = if (index == 8)
                    then True
            else if(isInt((words $ list) !! index))
                        then rnc (index + 1) list
                else False
{-
remove non number cahracters from a string
-}
reNonNum :: [Char] -> [Char]
reNonNum x = [c | c <- x, c `elem` ['0'..'9']]
{-
Verify if pattern matches given flag
-}



patt :: [Char] -> [Char]
patt ('(':a:',':b:',':c:')':xs)
    | ((length xs) == 0 && a `elem` ['A'..'H'] && b `elem`['A'..'H'] && c `elem`['1'..'9'])= "TNP"
    | otherwise = "No"
patt ('(':a:',':b:')':xs)
    | ((length xs) == 0 && a `elem` ['1'..'8'] && b `elem`['A'..'H'])= "FF"
    | ((length xs) == 0 && a `elem` ['A'..'H'] && b `elem`['A'..'H'])= "TNT"
    | otherwise = "No"
patt other = "No"
-- checks if args are in right order



sorted :: (Ord a) => [a] -> Bool
sorted [] = True
sorted [x] = True
sorted (x:y:xs) = if x < y then sorted (y:xs) else False



-- removes the new line / empty strings
removeEmpties :: Foldable t => [t a] -> [t a]
removeEmpties x = [ c | c <- x, length c /= 0]


-- split each section into a list of lists
splt :: Int -> [a] -> [Int] -> [[a]]
splt x list index = if x >= 0
                       then snd(splitAt (index !! x)  list):splt (x - 1) (fst $ splitAt (index !! x) list) index
               else []


countSpaces :: String -> Int
countSpaces str = length $ filter (== ' ') str
isInt x = case reads x :: [(Integer, String)] of
  [(_, "")] -> True
  _         -> False


-- gets a list of indicies as to where the sections are
getIn :: Int -> [[Char]] -> [Int]
getIn x list = if x < 6
                  then (fromMaybe (-1) $ elemIndex (sections !! x) list):getIn (x + 1) list
          else []



-- check if theres at least one newline between sections
leastone :: [Char] -> Int -> Bool
leastone args index = if (isInfixOf ('\n':'\n':(sections !! index)) (args) && (index < 6))
                         then leastone args (index + 1)
             else if (index == 6)
                 then True
                 else False



-- forceAssign :: String -> Int
-- forceAssign
tupleFuckery :: [[Char]] -> [[Char]]
tupleFuckery s = map splitTuple tail
    where head:tail = s
parseTuple = repl ' ' ','


splitTuple :: [Char] -> [Char]
splitTuple s = map parseTuple [ c| c <- s, c/=')' && c /= '(']


repl :: Char -> Char -> Char -> Char
repl c' c s = if s == c then c' else s


listToTuple :: [a] -> (a,a) -- [a,b] -> (a,b)
listToTuple (x:y:xs) = (x,y)


listToThrupple :: [a] -> (a,a,a)
listToThrupple (x:y:z:xs) = (x,y,z)

taskToCoord :: (String, String) -> (Int, Int)
taskToCoord (s,y)
    |  (s,"A") == (s,y) = (read s - 1, 0)
    |  (s,"B") == (s,y) = (read s - 1, 1)
    |  (s,"C") == (s,y) = (read s - 1, 2)
    |  (s,"D") == (s,y) = (read s -1 , 3)
    |  (s,"E") == (s,y) = (read s -1, 4)
    |  (s,"F") == (s,y) = (read s -1, 5)
    |  (s,"G") == (s,y) = (read s -1, 6)
    | otherwise = (read s -1, 7)
-- right side task to coord

righttaskToCoord :: (String, String) -> (String, Int)
righttaskToCoord (s,y)
    |  (s,"A") == (s,y) = (s, 0)
    |  (s,"B") == (s,y) = (s, 1)
    |  (s,"C") == (s,y) = (s, 2)
    |  (s,"D") == (s,y) = (s, 3)
    |  (s,"E") == (s,y) = (s, 4)
    |  (s,"F") == (s,y) = (s, 5)
    |  (s,"G") == (s,y) = (s, 6)
    | otherwise = (s, 7)
-- left side task to coord

lefttaskToCoord :: (String, Int) -> (Int, Int)
lefttaskToCoord (s,y)
    |  ("A",y) == (s,y) = (0,y)
    |  ("B",y) == (s,y) = (1,y)
    |  ("C",y) == (s,y) = (2,y)
    |  ("D",y) == (s,y) = (3,y)
    |  ("E",y) == (s,y) = (4,y)
    |  ("F",y) == (s,y) = (5,y)
    |  ("G",y) == (s,y) = (6,y)
    | otherwise = (7, y)

leftThruppleToCoord :: (String, Int, Int) -> (Int, Int, Int)
leftThruppleToCoord (s,r,y)
    |  ("A",r,y) == (s,r,y) = (0,r,y)
    |  ("B",r,y) == (s,r,y) = (1,r,y)
    |  ("C",r,y) == (s,r,y) = (2,r,y)
    |  ("D",r,y) == (s,r,y) = (3,r,y)
    |  ("E",r,y) == (s,r,y) = (4,r,y)
    |  ("F",r,y) == (s,r,y) = (5,r,y)
    |  ("G",r,y) == (s,r,y) = (6,r,y)
    | otherwise = (7,r,y)

rightThruppleToCoord :: (String, String, String) -> (String, Int, Int)
rightThruppleToCoord (s,r,y)
    |  (s,"A",y) == (s,r,y) = (s,0,read y)
    |  (s,"B",y) == (s,r,y) = (s,1,read y)
    |  (s,"C",y) == (s,r,y) = (s,2,read y)
    |  (s,"D",y) == (s,r,y) = (s,3,read y)
    |  (s,"E",y) == (s,r,y) = (s,4,read y)
    |  (s,"F",y) == (s,r,y) = (s,5,read y)
    |  (s,"G",y) == (s,r,y) = (s,6,read y)
    | otherwise = (s,7,read y)


{-Logic now works for 8x8 currenttly-}
testlist = [[0,0,0,0,0,0,0,0],
            [0,0,0,0,0,0,0,0],
            [0,0,0,0,0,0,0,0],
            [0,0,0,0,0,0,0,0],
            [0,0,0,0,0,0,0,0],
            [0,0,0,0,0,0,0,0],
            [0,0,0,0,0,0,0,0],
            [0,0,0,0,0,0,0,0]]



testTNP =  [[0,0,0,0,0,0,0,0],
            [0,0,0,0,0,0,0,0],
            [0,0,0,0,0,0,0,0],
            [0,0,0,0,0,0,0,0],
            [0,0,0,0,0,0,0,0],
            [0,0,0,0,0,0,0,0],
            [0,0,0,0,0,0,0,0],
            [0,0,0,0,0,0,0,0]]
currentPath = []
worstPath = [[0,0,0,0,0,0,0,0],[maxBound :: Int]]



-- Finds smallest penalty in a 2-D list after TNP
parseNode :: [[Int]] -> [[Int]] -> [Int] -> Int -> [[Int]]
parseNode list tnplist currentPath totalPen
    | taillist /= [] =
        returnLowPen [parseNode (safeAdd2DLst (editCols (-1) index taillist) (tnplist !! index)) tnplist  (addToPath index) (totalPen + (headlist !! index)) | index <- [0..7], (headlist !! index) /= (-1)]
    | otherwise = returnLowPen [assembleLeaf (addToPath index) ((addFirstTNP index) + (headlist !! index)) | index <- [0..7], (headlist !! index) /= -1, ((tnplist !! index) !! (currentPath !! 0)) /= (-1)]
    where   newTaillist n = safeAdd2DLst taillist n  --adds TNP to head of tail list
            headlist:taillist = list
            addToPath n = currentPath++[n]
            addFirstTNP n = (totalPen + ((tnplist !! n) !! (currentPath !! 0)))


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



--Forced Assignment
--Changes 2D array to reflect forced assignments as a -1
forceAssign :: [[Int]] -> [(Int, Int)] -> [[Int]]
forceAssign [] pairs = []
forceAssign twoDlist pairs = (newForceList x pairs (8-(length twoDlist))):(forceAssign xs pairs)
    where x:xs = twoDlist


--Change a list for all forced pairs
newForceList :: [Int] -> [(Int, Int)] -> Int -> [Int]
newForceList oneDlist pairs listIndex = foldl(\acc pair -> if (fst(pair) == listIndex) then (modList acc (snd(pair))) else acc) oneDlist pairs



--Returns a list with one element the same and -1s everywhere else
modList :: [Int] -> Int -> [Int]
modList oneDlist keepIndex = (replicate keepIndex (-1))++((oneDlist !! keepIndex):(replicate (7-keepIndex) (-1)))


--Change value
--Changes 2D array to arbitrary values in tuple
changeValue :: [[Int]] -> [(Int, Int, Int)] -> [[Int]]
changeValue [] triples = []
changeValue twoDlist triples = (newChangeList x triples (8-(length twoDlist))):(changeValue xs triples)
    where   x:xs = twoDlist

--Change a list for all change triples
newChangeList :: [Int] -> [(Int, Int, Int)] -> Int -> [Int]
newChangeList oneDlist triples listIndex = foldl(\acc trip -> if (trd1 trip == listIndex) then (modChangeList acc (trd2 trip) (trd3 trip)) else acc) oneDlist triples


--Returns a list with one element changed to arbitrary value
modChangeList :: [Int] -> Int -> Int -> [Int]
modChangeList oneDlist keepIndex newValue = (take keepIndex oneDlist)++(newValue:(drop (keepIndex+1) oneDlist))

--Forbidden Assignments
--Changes 2D array to reflect forbidden assignments
forbidAssign :: [[Int]] -> [(Int, Int)] -> [[Int]]
forbidAssign [] pairs = []
forbidAssign twoDlist pairs = (newForbidList x pairs (8-(length twoDlist))):(forbidAssign xs pairs)
    where x:xs = twoDlist


--Change a list for all forbidden pairs
newForbidList :: [Int] -> [(Int, Int)] -> Int -> [Int]
newForbidList oneDlist pairs listIndex = foldl(\acc pair -> if (fst(pair) == listIndex) then (modForbidList acc (snd(pair))) else acc) oneDlist pairs


--Returns a list with one element changed to -1
modForbidList :: [Int] -> Int -> [Int]
modForbidList oneDlist keepIndex = (take keepIndex oneDlist)++((-1):(drop (keepIndex+1) oneDlist))



--Checks if any conflicts exist returns 1 if valid, 0 if invalid
validCheck :: [(Int, Int)] -> Int
validCheck [] = 1
validCheck triples = if ((validCheckF triples == 1) && (validCheckS triples == 1)) then 1 else 0


--Checks if any conflicts exist within (F)irst assignment tuple
validCheckF :: [(Int, Int)] -> Int
validCheckF triples = if elem (-1) (foldl(\acc trip -> if (elem (fst(trip)) acc) then (-1):acc else (fst(trip)):acc) [] triples) then 0 else 1


--Checks if any conflicts exist within (S)econd assignment tuple
validCheckS :: [(Int, Int)] -> Int
validCheckS triples = if elem (-1) (foldl(\acc trip -> if (elem (snd(trip)) acc) then (-1):acc else (snd(trip)):acc) [] triples) then 0 else 1


trd1 :: (a,b,c) -> a
trd1 (x,y,z) = x
trd2 :: (a,b,c) -> b
trd2 (x,y,z) = y
trd3 :: (a,b,c) -> c
trd3 (x,y,z) = z



errPrnt :: Int -> String
errPrnt 0 = "File Not Found"
errPrnt 1 = "Wrong Number of Arguments"
errPrnt 2 = "partial assignment error"
errPrnt 3 = "invalid machine/task"
errPrnt 4 = "machine penalty error"
errPrnt 5 = "invalid task"
errPrnt 6 = "invalid penalty"
errPrnt 7 = "No valid solution possible!"
-- errPrnt 8 = "Integer not in Range"
errPrnt x = "Error while parsing input file"
