import System.Environment
import System.Exit
import System.IO
import Control.Exception
import Data.IORef
import Data.List
import Data.Maybe
import Data.Char
import Text.Read
import Debug.Trace
import SplashPrinter

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
       else do
            let file = (args !! 0) 
            x <- try (readFile file)
            case x of
                Left exc -> handler exc
                Right content -> pass (args !! 1) content

handler :: IOError -> IO ()
handler ex = putStrLn (errPrnt 0) >> exitFailure
--writeout :: FilePath -> Int -> IO ()
--writeout path x = writeFile path (errPrnt x)
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

pass :: [Char] -> [Char] -> IO ()
pass out content = do
    let context = totalStrip (lines (filter (/= '\r') content))
    let index = divIn 0 context
    let blocks = reverse (splt ((length index)-1) context index)
    let theGrid = GridPart (rowEstablish 1 (Rowpiece [0] Nullpiece)) (rowEstablish 1 (Rowpiece [0] Nullpiece))
    let parsedGrid = parseDriver 0 blocks [] -- print blocks to view the list
    case (last parsedGrid) of   (Err x) -> errorPrintOut out (errPrnt x) >> exitFailure
                                _ -> putStrLn "Congrats"
                                    --parsedGrid overMapping
    -- Catch error here.
    
{-
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
     -}
{-
takes the out stream and a list of strings to parse
sends each section off to their parsers
-}
parseDriver :: Int -> [[[Char]]] -> [IsObject] -> [IsObject]
parseDriver count content opList
    |   theresError opList   =  opList
    |   count == 0  = parseDriver 2 content (pushOnward count opList content)
    |   count == 2  = parseDriver 3 content (pushOnward count opList content)
    |   count == 3  = parseDriver 4 content (pushOnward count opList content)
    |   count == 4  = parseDriver 5 content (pushOnward count opList content)
    |   count == 5  = parseDriver 6 content (pushOnward count opList content)
    |   count == 6  = parseDriver 7 content (pushOnward count opList content)
    |   count == 7  = if (length content) > 7
                        then letError opList 10
                        else opList

pushOnward :: Int -> [IsObject] -> [[[Char]]] -> [IsObject]
pushOnward xOrder opList content
    | (length content) <= (xOrder)    = letError opList 10
    | xOrder == 0   = pname opList ((removeEmpties (content !! 0)) ++ (removeEmpties (content !! 1)))
    | xOrder == 2   = if (flagCheck (xOrder-1) (last (content !! (xOrder-1))) (head (content !! xOrder)))
        then pfpa opList (removeEmpties (content !! xOrder))
        else letError opList 10
    | xOrder == 3   = if (flagCheck (xOrder-1) (last (content !! (xOrder-1))) (head (content !! xOrder)))
        then pfm opList (removeEmpties (content !! xOrder))
        else letError opList 10
    | xOrder == 4   = if (flagCheck (xOrder-1) (last (content !! (xOrder-1))) (head (content !! xOrder)))
        then ptnt opList (removeEmpties (content !! xOrder))
        else letError opList 10
    | xOrder == 5   = if (flagCheck (xOrder-1) (last (content !! (xOrder-1))) (head (content !! xOrder)))
        then pmp opList (removeEmpties (content !! xOrder))
        else letError opList 10
    | xOrder == 6   = if (flagCheck (xOrder-1) (last (content !! (xOrder-1))) (head (content !! xOrder)))
        then ptnp opList (removeEmpties (content !! xOrder))
        else letError opList 10
    | otherwise     = letError opList 10


flagCheck :: Int -> [Char] -> [Char] -> Bool
flagCheck xOrder lastTail nextHead
    | nextHead /= (sections !! xOrder)  = False
    | lastTail /= ""                    = False
    | otherwise                         = True

{-
parseDriver :: p -> [[[Char]]] -> IO ()
parseDriver out content = do
     { pname out $ content !! 0
     ; pfpa out $ content !! 1
     ; pfm out $ content !! 2
     ; ptnt out $ content !! 3
     ; pmp out $ content !! 4
     ; ptnp out $ content !! 5
     ; let splitByWords = mapByWord $ content !! 4
     ; let (_:end) = splitByWords
     ; let listO'ListO'IntsOH = mapFullToInt end
     -- ; pmp out $ content !! 4
     ; ptnp out $ content !! 5
     --; print $ content
     ; print listO'ListO'IntsOH
     -- ; print $ content
     }
-}

mapByWord :: [String] -> [[String]]
mapByWord l = map words l
mapToInt :: [String] -> [Int]
mapToInt l = map read l
mapFullToInt :: [[String]] -> [[Int]]
mapFullToInt l = do
  w <- l
  return $ mapToInt w

-- Just triggers errPrnt 10, nothing else.
inFault :: [Char] -> IO ()
inFault out = errorPrintOut out (errPrnt 10) >> exitFailure
{-
checks if theres more than one name or if theres 2 names separated by a ' '
prints parse error and exits if true
-}
pname :: [IsObject] -> [[Char]] -> [IsObject]
pname opList l = if ((length l) /= 2 || ' ' `elem` (l !! 1))
            then letError opList 10
            else if (l !! 0) /= (sections !! 0)
                then letError opList 10
                else opList
{-
If the section only contains the name then return
else send the recursive checker to see if all arguments have the correct
pattern. Uses Flag FF (forced/forbidden)
if one line is in correct Prints invalid machine/task and exits
-}
pfpa :: [IsObject] -> [[Char]] -> [IsObject]
pfpa opList l = if ((length l) == 1)
            then opList
            else buildOps l 1 "FA" opList
                --rc' l 1 "FF"
                --then return()
                --else putStrLn (errPrnt (testIF l 1 "FF")) >> exitFailure
        --writeout out (error code) >> exitFailure
{-
Exactly the Same as Above
-}
pfm :: [IsObject] -> [[Char]] -> [IsObject]
pfm opList l = if ((length l) == 1)
           then opList
           else buildOps l 1 "FM" opList
           --writeout out 3 >> exitFailure
{-
if section only contains the name return
else recursively check using TNT flag
if true return else print invalid task decsription and exit
-}
ptnt :: [IsObject] -> [[Char]] -> [IsObject]
ptnt opList l = if ((length l) == 1)
            then opList
            else buildOps l 1 "TNT" opList
        --writeout out 5 >> exitFailure
{-
uses its out recursive checker, No flag
if checker returns true then return else print the associated error and exit
invalid penalty or machine penalty error
-}
-- check number of occurences of ' '
-- check length $ words line
--
pmp :: [IsObject] -> [[Char]] -> [IsObject]
pmp opList l = if ((length l) == 1)
           then letError opList 4
           else buildGrid l 1 opList
{-
if section only contains header return
else recursively check using TNP flag
if true return else print invalid task decription and exit
-}
ptnp :: [IsObject] -> [[Char]] -> [IsObject]
ptnp opList l = if ((length l) == 1)
            then opList
            else buildOps l 1 "TNP" opList
        --writeout out 5 >> exitFailure
------------------------------------------------------------------------------------
{-
recursive checker
takes a list an idex to start from and a verifier to check agasint
checks if each line in that secion conforms to its pattern
-}
rc' :: [[Char]] -> Int -> [Char] -> Bool
rc' arg index verifier =
    if (index < (length arg) && ((patt $ arg !! index) == verifier))
        then rc' arg (index + 1) verifier
        else if index == length arg
            then True
            else False
{-
recursive checker for machine penalty
checks for length and negatives
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
removeEmpties :: [[Char]] -> [[Char]]
removeEmpties [] = []
removeEmpties (x:xs)
    | x == ""   = removeEmpties xs
    | otherwise = [x] ++ (removeEmpties xs)
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
-- returns a string to be printed
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


-- Test if the data in question contains extranous pieces that is considered entirely invalid. (Not letter, not number)
buildGrid :: [[Char]] -> Int -> [IsObject] -> [IsObject]
buildGrid arg index opList =
    let newDex = if index > 8
                then 8
                else index
        target = vetLine (linar (arg !! newDex))
    in  if (index < (length arg) && (0 == target))
            then buildGrid arg (index + 1) (lexOps opList (arg !! index) "MP")
            else if index == 9
                then opList
                else if 0 == target
                    then letError opList 4
                    else letError opList target

buildOps :: [[Char]] -> Int -> [Char] -> [IsObject] -> [IsObject]
buildOps arg index verifier opList = 
    let newDex = if index == (length arg)
                    then (index-1)
                    else index
        target = isItOtherError (comber (stripBracket (arg !! newDex))) verifier
    in  if (index < (length arg) && (0 == target))
            then buildOps arg (index + 1) verifier (lexOps opList (arg !! index) verifier)
            else if index == length arg
                then opList
                else letError opList target

lexOps :: [IsObject] -> [Char] -> [Char] -> [IsObject]
lexOps opList chs verifier =
    let output = read (verifier ++ " " ++ chs) :: IsObject
    in case output of   (FA (x,y))    -> if (fpaConflict output opList)
                                            then letError opList 2
                                            else (opList ++ [output])
                        _           -> (opList ++ [output])

fpaConflict :: IsObject -> [IsObject] -> Bool
fpaConflict x [] = False
fpaConflict (FA (a,b)) ((FA (x,y)):xs)
    | a == x    = True
    | b == y    = True
    | otherwise = fpaConflict (FA (a,b)) xs

-- Look for letter starting texts to set up the indexes, 0 and 1 should be Name: and corresponding name string.
divIn :: Int -> [[Char]] -> [Int]
divIn x []       = []
divIn x ("":rest)   = divIn (x+1) rest
divIn x (a:rest)    = case isLetter (head a) of
    False   -> divIn (x+1) rest
    True    -> x:(divIn (x+1) rest)

-- Checks if machine penalty line given is valid.
vetLine :: [[Char]] -> Int
vetLine xPenal
    | "" `elem` xPenal          = 10
    | " " `elem` xPenal         = 10
    | length xPenal /= 8        = 4
    | allPenal xPenal == False  = 6
    | otherwise                 = 0

-- Checks if other error maybe triggering instead of default error.
isItOtherError :: [[Char]] -> [Char] -> Int
isItOtherError [a,b,c] "TNP"
    | ' ' `elem` (a ++ b ++ c)  = 10
    | [] `elem` [a,b,c]         = 10
    | isTask a == False         = 5
    | isTask b == False         = 5
    | areDigits c == False      = 6
    | otherwise                 = 0
isItOtherError [a,b] ('F':_)
    | ' ' `elem` (a ++ b)       = 10
    | [] `elem` [a,b]           = 10
    | isMachine a == False      = 3
    | isTask b == False         = 3
    | otherwise                 = 0
isItOtherError [a,b] "TNT"
    | ' ' `elem` (a ++ b)       = 10
    | [] `elem` [a,b]           = 10
    | isTask a == False         = 3
    | isTask b == False         = 3
    | otherwise                 = 0
isItOtherError format error = 10

stripBracket :: [Char] -> [Char]
stripBracket x = 
    case (head x, last x) of    ('(',')') -> init (tail x)
                                _         -> "format error"

isTask :: [Char] -> Bool
isTask [] = False
isTask (x:xs)
    | length xs > 0             = False
    | x `notElem` ['A'..'H']    = False
    | otherwise                 = True

isMachine :: [Char] -> Bool
isMachine [] = False
isMachine (x:xs)
    | length xs > 0             = False
    | x `notElem` ['1'..'8']    = False
    | otherwise                 = True

allPenal :: [[Char]] -> Bool
allPenal [] = True
allPenal (x:xs) = case (areDigits x) of
    True  -> allPenal xs
    False -> False

areDigits :: [Char] -> Bool
areDigits [] = True
areDigits (x:xs) = case (isDigit x) of 
    True  -> areDigits xs
    False -> False

stripEOL :: [Char] -> [Char]
stripEOL xline = dropWhileEnd isSpace xline

totalStrip :: [[Char]] -> [[Char]]
totalStrip [] = []
totalStrip (x:xs) = (stripEOL x):(totalStrip xs)

-- Minor modificated implementation [Prelude.list.lines], breaks up string based on comma.
-- Source: https://hackage.haskell.org/package/base-4.10.1.0/docs/src/Data.OldList.html#lines
comber :: [Char] -> [[Char]]
comber "" =  [" "]
comber s  =  cons (case break (== ',') s of
                    (l, s') -> (l, case s' of
                                        []      -> []
                                        _:s''   -> comber s''))
        where
            cons ~(h, t) =  h : t

linar :: [Char] -> [[Char]]
linar "" = [" "]
linar s  =  cons (case break (== ' ') s of
                    (l, s') -> (l, case s' of
                                        []      -> []
                                        _:s''   -> linar s''))
        where
            cons ~(h, t) =  h : t


rowEstablish :: Int -> Rowpiece -> Rowpiece
rowEstablish x y
    | x > 7  = y
    | x <= 7 = Rowpiece [x] (rowEstablish (x+1) y)
rowEstablish _ _ = Rowpiece [1] Nullpiece

{-
splashParse :: [String] -> AObject -> Either AObject AGrid
splashParse ("!!!":xs) = Left (ErObject "error" xs)
splashParse (x:xs) =
    case (dropWhileEnd isSpace x) of
        ("") -> splashParse xs
        ("Name:") -> splashParse (vetName xs)
        --("forced partial assignment:") ->
        --("forbidden machine:") ->
        --("too-near tasks:") ->
        --("machine penalties:") ->
        --("too-near penalities") ->
        ("Correct") -> Right (MPGrid [1,1,1])
        _ -> splashParse ["!!!", (errCode "inFault")]
-}

data AGrid = GridPart Rowpiece Rowpiece
data Rowpiece = Rowpiece [Int] Rowpiece | Nullpiece | Errorpiece Int deriving (Show, Eq)

letError :: [IsObject] -> Int -> [IsObject]
letError opList x = opList ++ [(Err x)]

theresError :: [IsObject] -> Bool
theresError [] = False
theresError x = 
    case (last x) of    (Err c) -> True
                        x       -> False

data AObject = MapObject {  row :: Int, 
                            column :: Int, 
                            score :: Int } | ErObject String deriving (Show)
            -- Describe transformation and assignment. always size of three: (task, task, score)

            -- Describe transformation and assignment. always size of three: (machine, task, score)
            -- (_:_:(0>): Penalty assignment flag.
            -- (_:_:-1): FPA assignment, triggers function which maps -2
            -- (_:_:-2): Forbidden Square. Mapping -1 into this square triggers "fpa". Mapped as side effect to FPA or directly by FM. (FM is parsed after FPA, thus it shouldn't matter.)
            -- (_:_:-3): Triggers Invalid Penalty.
            -- Others are execution states.

-- Define newtype (alias), create smart constructor. Deriving Syntax

-- This set up allows text to be directly read using following context.
-- readMaybe "MPObject (1,A)" :: IsObject
-- readMaybe "TNObject (A,A)" :: IsObject
-- readMaybe "FXObject (A,B,1)" :: IsObject
data IsObject = FA (Int,Lexeme) | FM (Int,Lexeme) | TNT (Lexeme,Lexeme) | TNP (Lexeme,Lexeme,Int) | Err Int | MP Int Int Int Int Int Int Int Int deriving (Read)

instance Show IsObject where
    show (FA (x, Ident y)) = show ((x-1),(tParse y))
    show (FM (x, Ident y)) = show ((x-1),(tParse y))
    show (TNT ((Ident x), (Ident y))) = show ((tParse x),(tParse y))
    show (TNP ((Ident x), (Ident y), z)) = show ((tParse x),(tParse y),z)
    show (MP a b c d e f g h)   = show (a,b,c,d,e,f,g,h)
    show _ = show "Invalid"

tParse :: String -> Int
tParse x =
    case x of   "A" -> 0
                "B" -> 1
                "C" -> 2
                "D" -> 3
                "E" -> 4
                "F" -> 5
                "G" -> 6
                "H" -> 7
                x   -> -1