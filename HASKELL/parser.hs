import Data.Array
import System.Environment
import System.Exit
import System.IO
import Control.Exception
import Data.IORef
import Data.List
import Data.Maybe

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
strToArr :: String  -> Array Int Char
strToArr s = listArray (0, length s - 1) s

lsCom2 = [(x,y) |x <- [1..5], y <- [1..5], x * y == 25]
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]
noSpace s = [c | c <- s, c `elem` ['0'..'9']]

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
       	    putStrLn $ errPrnt 0
	    exitFailure
       else case init args of 
              [file] -> do
              	x <- try $ readFile file
		case x of
		  Left exc -> handler exc
		  Right content -> print $ lines content
				-- print $ getIn 0 $ lines content
		  		-- print $ lines content -- lines in a list
		    		-- llst content
		  		-- writeout (args !! 1) 0
		  		-- writeFile (args !! 1)  content
		  		-- putStr content (writes to stdIO)

handler :: IOError -> IO ()
handler ex = putStrLn (errPrnt 0) >> exitFailure

-- Does some dumb shit ignore its a test
llst cnt = do
     let l = map ((!!) (lines cnt)) [0,1..(length (lines cnt) - 1)]
     let line = l !! 0
     putStr line
     return line

-- adWrite = 1:(2:(3:(4:(5:[]))))
-- gets a list of indicies as to where the sections are
getIn x list = if x < 6
      	       	  then (fromMaybe (-1) $ elemIndex (sections !! x) list):getIn (x + 1) list
		  else []


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

-- writeout :: Int ->
writeout path x = writeFile path (errPrnt x)

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









