import System.Environment
import System.IO
import System.Exit
import Data.List
import Data.Char
import Data.String
import Data.Maybe
import Data.Either
import Text.Read
import Text.Read.Lex
import GHC.TypeLits
import GHC.Exts

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
    
    
    let establish = rowEstablish 1 Nullpiece
    --let masterGrids = splashParse inputContents
    putStrLn "Hello, World!"


--splashParse :: [String] ->  -> AGrid
--splashParse ()

rowEstablish :: Int -> Rowpiece -> Rowpiece
rowEstablish x Nullpiece = rowEstablish (x+1) (Rowpiece [0,0,0,0,0,0,0,0] Nullpiece)
rowEstablish 8 (Rowpiece a next) = Rowpiece a next
rowEstablish x (Rowpiece a next) = Rowpiece a (rowEstablish (x+1) next)

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

data AGrid = GridPart Rowpiece Rowpiece deriving (Show)
data Rowpiece = Rowpiece [Int] Rowpiece | Nullpiece deriving (Show, Eq)


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
data IsObject = MPObject (Lexeme,Lexeme) | TNObject (Lexeme,Lexeme) | FXObject (Lexeme,Lexeme,Int) deriving (Read)
data IsPenLine = PenLine Int Int Int Int Int Int Int Int deriving (Show, Read)

instance Show IsObject where
    show (MPObject (Number x, Ident y)) =
        let out = numberToInteger x
        in case out of  (Just x) -> show (x,y)
                        Nothing  -> show (-1,-1)
    show (TNObject ((Ident x), (Ident y))) = show ((tParse x),(tParse y))
    show (FXObject ((Ident x), (Ident y), z)) = show ((tParse x),(tParse y),z)
    show _ = show "Invalid"
-- Before parsing, check if isSpace is element of the string. (After eol strip)
    -- object that looks like (1,"A") will trigger exception for both readEither and readMaybe.
    -- So check '"', ',', and ' ' before applying readEither x :: Either String IsObject
        -- If any of them are found, returns Infault. (Diff, " is captured as infault instead of nullMT)
-- Special characters (Not letter or number) should be shown with (Symbol x)
-- Numbers should be shown with (MkDecimal x)
-- characters were inputted as Machine or Task.

{-
-- file: ch06/eqclasses.hs
instance Read Color where
-- readsPrec
 is the main function for parsing input
readsPrec _ value =
-- We pass tryParse a list of pairs. Each pair has a string
-- and the desired return value. tryParse will try to match
-- the input to one of these strings.
tryParse [("Red", Red), ("Green", Green), ("Blue", Blue)]
where tryParse [] = []
-- If there is nothing left to try, fail
tryParse ((attempt, result):xs) =
-- Compare the start of the string to be parsed to the
-- text we are looking for.
if (take (length attempt) value) == attempt
-- If we have a match, return the result and the
-- remaining input
then [(result, drop (length attempt) value)]
-- If we don't have a match, try the next pair
-- in the list of attempts.
else tryParse xs
-}


{-
gridMapper :: AObject -> AGrid -> Either AGrid AnError
gridMapper (MPObject x y (-3)) (MPGrid grid) = 
    let (left, right) = splitAt (8*x + y) grid
    in MPGrid (left ++ [0] ++ (tail right))
gridMapper (MPObject x y (-1)) (MPGrid grid) =  
    case (grid !! 8*x+y) of -2 ->   splashParse ["!!!", (errCode "fpa")]
                            _ ->    gridMapper (MPObject x y (-3)) (applyFPA x y (MPGrid grid))
gridMapper (MPObject x y (-2)) (MPGrid grid) =  
    let (left, right) = splitAt (8*x + y) grid
    in MPGrid (left ++ [-2] ++ (tail right))
-}

-- readMaybe "MPObject (1,A)" :: Either String IsObject

stripEOL :: String -> String
stripEOL xline = dropWhileEnd isSpace xline

checkBracket :: String -> String
checkBracket x = 
    case (head x, last x) of    ('(',')') -> x      -- was [init (tail x)] to strip bracket.
                                _         -> "inFault"

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

mParse :: String -> Int
mParse x = 
    case x of   "1" -> 0
                "2" -> 1
                "3" -> 2
                "4" -> 3
                "5" -> 4
                "6" -> 5
                "7" -> 6
                "8" -> 7
                x   -> -1

pParse :: String -> Int
pParse x =
    case (isPenalty x) of   True -> read x :: Int
                            False -> -1

isPenalty :: String -> Bool
isPenalty ""  = False
isPenalty xs  =
  case dropWhile isDigit xs of
    ""       -> True
    _        -> False


parseError x = do
    putStrLn x

errCode :: String -> String
errCode "inFault" = "Error while parsing input file"
errCode "fpa" = "partial assignment error"
errCode "mp" = "machine penalty error"
errCode "nullMT" = "invalid machine/task"
errCode "nullT" = "invalid task"
errCode "nullP" = "invalid penalty"
errCode "noSol" = "No valid solution possible!"
errCode x = "Invalid Error Called"

-- Minor modificated implementation [Prelude.list.lines], breaks up string based on comma.
-- Source: https://hackage.haskell.org/package/base-4.10.1.0/docs/src/Data.OldList.html#lines
comber :: String -> [String]
comber "" =  []
comber s  =  cons (case break (== ',') s of
                    (l, s') -> (l, case s' of
                                        []      -> []
                                        _:s''   -> lines s''))
        where
            cons ~(h, t) =  h : t
