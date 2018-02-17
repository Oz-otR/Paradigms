import Data.Array
import System.Environment
import System.Exit
import System.IO
import Control.Exception

sections = ["Name:"
            ,"forced partial assignment:"
            ,"forbidden machine:","too-near tasks:"
            ,"machine penalties:"
            ,"too-near penalities:"
            ,"\n"]

penalty = "1 1 1 1 1 1 1 1\n"

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

main = handle handler $ do
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
		  Right content -> putStr content

handler :: IOError -> IO ()
handler ex = putStrLn (errPrnt 0) >> exitFailure

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









