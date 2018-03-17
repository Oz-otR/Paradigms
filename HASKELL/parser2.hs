import System.Environment
import System.Exit
import System.IO
import Control.Exception
import Data.IORef
import Data.List
import Data.Maybe
import Data.Char

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
     -- ; print $ content
     }

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
                else putStrLn (errPrnt 5) >> exitFailure
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
		         else if ((countSpaces $ args !! index) == 7 && (length $ words $ (args !! index)) == 8)
		             then 4
			     else if(rnc 0 $ args !! index)
			         then mprc args (index +1)
				 else 4
-------------------------------------------------------------------------------------------

rnc :: Int -> String -> Bool
rnc index list = if (isInt ((words $ list) !! index) && index < 8)
    	       	    then rnc (index + 1) list
		    else if (index == 8)
		    	then True
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

-- returns a string to be printed
errPrnt :: Int -> String
errPrnt 0 = "File Not Found"
errPrnt 1 = "Wrong Number of Arguments"
errPrnt 2 = "Partial Assignment Error"
errPrnt 3 = "Invalid Machine/Task"
errPrnt 4 = "Machine Penalty Error"
errPrnt 5 = "Invalid Task Description"
errPrnt 6 = "Invalid Penalty"
errPrnt 7 = "No Valid Solution"
-- errPrnt 8 = "Integer not in Range"
errPrnt x = "Error while Parsing File"
