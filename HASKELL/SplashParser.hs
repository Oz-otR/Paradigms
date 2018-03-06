import System.Environment
import System.Exit
import System.IO
import Control.Exception
import Data.IORef
import Data.List
import Data.Maybe
import Data.Char

sections = ["Name:"
            ,"forced partial assignment:"
            ,"forbidden machine:"
	    ,"too-near tasks:"
            ,"machine penalties:"
            ,"too-near penalities"
            ,"\n"]

main = turn

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

-- writeout :: Int ->
writeout path x = writeFile path (errPrnt x)

pass out content = do
     { if not(leastone (filter (/= '\r') content) 1)
          then putStrLn (errPrnt 10) >> exitFailure
	  else return()
     ; let pruned = removeEmpties $ lines $ filter (/= '\r') content
     ; let index = getIn 0 pruned
     ; print $ content
     ; if not(sorted index && not ((-1) `elem` index))
       	  then putStrLn (errPrnt 10) >> exitFailure
	  else return ()
     ; let blocks = reverse $ splt 5 pruned index
     ; parseDriver out blocks -- print blocks to view the list
     }

parseDriver out content = do
     { pname out $ content !! 0
     ; pfpa out $ content !! 1
     ; pfm out $ content !! 2
     ; ptnt out $ content !! 3
     ; pmp out $ content !! 4
     ; ptnp out $ content !! 5
     }

-- Parse Name -> DONE
pname out l = if ((length l) /= 2 || ' ' `elem` (l !! 1) )
             then putStrLn (errPrnt 10) >> exitFailure
	     else return()
	     --writeout out 10 >> exitFailure

-- Parse Forced -> DONE         
pfpa out l = if ((length l) == 1)
            then return ()
            else if rc' l 1 "FF"
                then return()
                else putStrLn (errPrnt 3) >> exitFailure
		--writeout out 3 >> exitFailure

-- Parse Forbidden -> DONE
pfm out l = if ((length l) == 1)
           then return()
           else if rc' l 1 "FF"
               then return()
               else putStrLn (errPrnt 3) >> exitFailure
	       --writeout out 3 >> exitFailure

-- Parse TNT -> DONE
ptnt out l = if ((length l) == 1)
            then return ()
            else if rc' l 1 "TNT"
                then return()
	        else putStrLn (errPrnt 5) >> exitFailure
		--writeout out 5 >> exitFailure

-- Parse Penalties -> wip
pmp out l = if ((length l) /= 9)
           then putStrLn (errPrnt 4) >> exitFailure
           else if (mprc l 1 == 0)
	       then return()
	       else putStrLn (errPrnt $ mprc l 1) >> exitFailure
	       --writeout out (mprc l 1) >> exitFailure

-- Parse TNP -> DONE
ptnp out l = if ((length l) == 1)
            then return ()
            else if rc' l 1 "TNP"
                then return()
                else putStrLn (errPrnt 5) >> exitFailure
		--writeout out 5 >> exitFailure

-- recursive checker
rc' arg index verifier = if (index < (length arg) && ((patt $ arg !! index) == verifier))
    	      	       	    then rc' arg (index + 1) verifier
			    else if index == length arg
			        then True
				else False

-- checks for neg and col length
mprc args index = if index > 8
     	  	     then 0
		     else if (isInfixOf "-" $ args !! index)
                         then 6
		         else if ((length $ reNonNum $ args !! index) == 8)
		             then mprc args (index + 1)
			     else 4

reNonNum x = [c | c <- x, c `elem` ['0'..'9']]
		     	 
-- Check for (1..8,A..H) pattern
patt ('(':a:',':b:',':c:')':xs)
    | ((length xs) == 0 && a `elem` ['A'..'H'] && b `elem`['A'..'H'] && c `elem`['1'..'8'])= "TNP"
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
removeEmpties x = [ c | c <- x, length c /= 0]

-- split each section into a list of lists
splt x list index = if x >= 0
       	    	       then snd(splitAt (index !! x)  list):splt (x - 1) (fst $ splitAt (index !! x) list) index
		       else []
-- gets a list of indicies as to where the sections are
getIn x list = if x < 6
      	       	  then (fromMaybe (-1) $ elemIndex (sections !! x) list):getIn (x + 1) list
		  else []

errPrnt :: Int -> String
errPrnt 0 = "File Not Found"
errPrnt 1 = "Wrong Number of Arguments"
errPrnt 2 = "Partial Assignment Error"
errPrnt 3 = "Invalid Machine/Task"
errPrnt 4 = "Machine Penalty Error"
errPrnt 5 = "Invalid Task Description"
errPrnt 6 = "Invalid Penalty"
errPrnt 7 = "No Valid Solution"
errPrnt 8 = "Integer not in Range"
errPrnt x = "Error while Parsing File"

leastone args index = if (isInfixOf ('\n':'\n':(sections !! index)) (args) && (index < 6))
       	    	         then leastone args (index + 1)
			 else if (index == 6)
			     then True
			     else False

