import System.Environment
import System.IO
import System.Exit
import Data.List
import Data.Char
import Data.String

main = do
    fileNames <- getArgs -- Obtain Arguments
    if length fileNames /= 2
        then do
            error "Please initiate program in following syntax.\n./JobShop (inputName) (outputName)"
        else do
            fileHandle <- openFile (fileNames !! 0) ReadMode
            tempHolder <- hGetContents fileHandle
            let inputContents = lines tempHolder
            hClose fileHandle

    --let masterGrids = splashParse inputContents
    putStrLn "Hello, World!"

{-}
splashParse :: (Show txtIn) => [txtIn] -> Maybe JobGrid
splashParse ("!!!":xs) = parseError xs
splashParse (x:xs) =
    case (dropWhileEnd isSpace x) of ("") -> splashParse xs
        ("Name:") -> 
        ("forced partial assignment:") ->
        ("forbidden machine:") ->
        ("too-near tasks:") ->
        ("machine penalties:") ->
        ("too-near penalities") ->
        _ -> ["!!!", (errCode "inFault")]
 -}       

--data MPGrid = [[Int]] deriving (Show)
--data TNGrid = [[Int]] deriving (Show)

--data TNObject = [Int]
            -- Describe transformation and assignment. always size of three: (task, task, score)
--data MPObject = MPObject Int Int Int
            -- Describe transformation and assignment. always size of three: (machine, task, score)
            -- (_:_:(0>)): Penalty assignment flag.
            -- (_:_:-255): FPA assignment flag, triggers function which maps -2
            -- (_:_:-65535): Forbidden Square. Mapping -1 into this square triggers "fpa". Mapped as side effect to FPA or directly by FM.

vetName :: [String] -> [String]
vetName ("#":x:xs) =
    case (dropWhileEnd isSpace x) of "" ->  (case (dropWhileEnd isSpace (xs !! 0)) of 
                                            "" -> vetName xs
                                            "forced partial assignment:" -> xs
                                            _ -> ["!!!", (errCode "inFault")])
vetName (x:xs) = 
    case y of "" -> vetName xs
              y -> vetName (["#"] ++ xs)
    where y = dropWhileEnd isSpace x

parseError :: String -> IO ()
parseError x = putStrLn x

errCode :: String -> String
errCode "inFault" = "Error while parsing input file"
errCode "fpa" = "partial assignment error"
errCode "mp" = "machine penalty error"
errCode "nullMT" = "invalid machine/task"
errCode "nullT" = "invalid task"
errCode "nullP" = "invalid penalty"
errCode "noSol" = "No valid solution possible!"
errCode x = "Invalid Error Called"
errCode _ = "Missing Error Description"


{-

vetName :: (Show txtIn) => [txtIn] -> ([txtIn], Bool) -> [txtIn]
vetName (x:xs) = 
    case (dropWhileEnd isSpace x) of ("") -> vetName xs
                                     _    -> vetName xs True
vetName (x:xs) True =
    case (dropWhileEnd isSpace x) of ("") ->  case (dropWhileEnd isSpace (xs !! 0)) of ("") -> vetName xs
                                                                                    ("forced partial assignment:") -> xs
                                                                                    _ -> parseError "inFault"


splashFormat :: (Show txtIn) => [txtIn] -> [String]
splashFormat [] = --Nothing?
splashFormat (x:xs) = 



lines                   :: String -> [String]
lines ""                =  []
-- Somehow GHC doesn't detect the selector thunks in the below code,
-- so s' keeps a reference to the first line via the pair and we have
-- a space leak (cf. #4334).
-- So we need to make GHC see the selector thunks with a trick.
lines s                 =  cons (case break (== '\n') s of
                                    (l, s') -> (l, case s' of
                                                    []      -> []
                                                    _:s''   -> lines s''))
  where
    cons ~(h, t)        =  h : t


wordsFB :: ([Char] -> b -> b) -> b -> String -> b
wordsFB c n = go
  where
    go s = case dropWhile isSpace s of
             "" -> n
             s' -> w `c` go s''
                   where (w, s'') = break isSpace s'


dropWhileEnd :: (Char -> Bool) -> Text -> Text
dropWhileEnd p t@(Text arr off len) = loop (len-1) len
  where loop !i !l | l <= 0    = empty
                   | p c       = loop (i+d) (l+d)
                   | otherwise = Text arr off l
            where (c,d)        = reverseIter t i

strTrimEnd :: String -> String
strTrimEnd (xs:x) =
    case x of isSpace -> strTrimEnd xs
-}