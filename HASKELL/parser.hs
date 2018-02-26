import Data.Array
import System.Environment
import System.Exit
import System.IO
import Control.Exception
import Data.IORef
import Data.List
import Data.Maybe
import Control.Monad

type Counter = Int -> IO Int

sections = ["Name:"
            ,"forced partial assignment:"
            ,"forbidden machine:"
	    ,"too-near tasks:"
            ,"machine penalties:"
            ,"too-near penalities:"
            ,"\n"]

-- strtoA :: String -> Array
-- strtoA x = listArray(0, length x - 1) x
-- stringToArray (sections !! 0)!0 => 'N'
-- strToArr :: String  -> Array Int Char
-- strToArr s = listArray (0, length s - 1) s

-- lsCom2 = [(x,y) |x <- [1..5], y <- [1..5], x * y == 25]
-- removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]
-- noSpace s = [c | c <- s, c `elem` ['0'..'9']]

eqs4 = do putStrLn "What is 2 + 2?"
          x <- readLn
          if x == 4
              then putStrLn "You're right!"
              else putStrLn "You're wrong!"

main = turn

turn = handle handler $ do 
--    counter <- mkCntr
--    update counter
--    errPrnt (get counter)

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
				-- print $ getIn 0 $ lines content
		  		-- print $ lines content -- lines in a list
		    		-- llst content
		  		-- writeout (args !! 1) 0
		  		-- writeFile (args !! 1)  content
		  		-- putStr content (writes to stdIO)

handler :: IOError -> IO ()
handler ex = putStrLn (errPrnt 0) >> exitFailure

-- writeout :: Int ->
writeout path x = writeFile path (errPrnt x)

pass out content = do
     {let pruned = removeEmpties $ lines content
     ; let index = getIn 0 pruned
     ; if not(sorted index && not ((-1) `elem` index))
       	  then putStrLn (errPrnt 10) >> exitFailure
	  else return ()
     ; let blocks = reverse $ splt 5 pruned index
     ; parseDriver out blocks -- print blocks to view the list
     -- $ sorted index && not ((-1) `elem` index) -- chk all cat in odr
     }

parseDriver out content = do
     { pname $ content !! 0
     ; print $ content !! 1
     --; pfpa $ content !! 1
     --; pfm content !! 2
     --; ptnt content !! 3
     --; pmp content !! 4
     --; ptnp content !! 5
     }
-- print $ length $ content !! 0


-- Parse Name -> DONE
pname l = if ((length l) /= 2 || ' ' `elem` (l !! 1) )
             then putStrLn (errPrnt 10) >> exitFailure
	     else return()

-- Parse Forced -> IP	          
-- pfpa l = if ((length l) == 1)
--   	       then return ()
--	       else 

-- Parse Forbidden -> IP
-- pfm ('(':_:',':_:')':xs) = 
-- pfm _ = putStrLn (errPrnt 10) >> exitFailure

-- Parse TNT -> IP
-- ptnt l = if ((length l) == 1)
--             then return ()
--             else

-- Parse Penalties -> IP
-- pmp l = if ((length l) == 1)
--             then return ()
--             else

-- Parse TNP -> IP
-- ptnp l = if ((length l) == 1)
--             then return ()
--             else


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

-- split each section into a list of lists
splt x list index = if x >= 0
       	    	       then snd(splitAt (index !! x)  list):splt (x - 1) (fst $ splitAt (index !! x) list) index
		       else []

-- adWrite = 1:(2:(3:(4:(5:[]))))
-- gets a list of indicies as to where the sections are
getIn x list = if x < 6
      	       	  then (fromMaybe (-1) $ elemIndex (sections !! x) list):getIn (x + 1) list
		  else []

-- Return nth element from a 6 tuple
get0 (a,_,_,_,_,_) = a -- needed as fst and snd only work for pairs
get1 (_,a,_,_,_,_) = a -- 
get2 (_,_,a,_,_,_) = a
get3 (_,_,_,a,_,_) = a
get4 (_,_,_,_,a,_) = a
get5 (_,_,_,_,_,a) = a

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

removeEmpties x = [ c | c <- x, length c /= 0]

-- EXPERIMENTAL

--when example
-- when (not(sorted index && not ((-1) `elem` index))) (putStrLn $ errPrnt 10)


-- Global Counter >> IMPURE: dont know if ill need it
mkCntr :: IO Counter
mkCntr = do
    r <- newIORef 0
    return (\i -> do modifyIORef r (+i)
                     readIORef r)
		     
update :: Counter -> IO ()
update counter = do
    a <- counter 1
    print [a]

get :: Counter -> IO ()
get counter = do
    a <- counter 0
    print a

-- Does some dumb shit ignore its a test
llst cnt = do
     let l = map ((!!) (lines cnt)) [0,1..(length (lines cnt) - 1)]
     let line = l !! 0
     putStr line
     return line



