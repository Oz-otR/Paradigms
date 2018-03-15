
ioVerify :: (Show argList) => [argList] -> Bool
ioVerify (x:y:[]) = True
ioVerify x = error "Please initiate program in following syntax.\n./JobShop (inputName) (outputName)"

pairProcessor :: String -> Int -> AObject
pairProcessor x pNum = 
    let xTest = comber (stripBracket (stripEOL x))
    in case xTest of    [a,b]           -> MapObject (mtParse a) (mtParse b) pNum
                        [a,b,c]         -> MapObject (mtParse a) (mtParse b) pNum --Temporary
                        _               -> ErObject "inFault"

mtParse :: String -> Int
mtParse [x]
    | elem x ['0'..'9'] = (digitToInt x) - 1
    | elem x ['A'..'Z'] = ord x
    | otherwise = -127
mtParse _ = -127


vetName :: [String] -> [String]
vetName ("[#>":x:xs) =
    case (dropWhileEnd isSpace x) of "" ->  (case (dropWhileEnd isSpace (xs !! 0)) of 
                                            "" -> vetName xs
                                            "forced partial assignment:" -> xs
                                            _ -> ["!!!", (errCode "inFault")])
vetName (x:xs) = 
    case y of "" -> vetName xs
              y -> vetName (["[#>"] ++ xs)
    where y = dropWhileEnd isSpace x


applyFPA :: Int -> Int -> AGrid -> AGrid
applyFPA x y (MPGrid grid) =
    MPGrid (applyFPA2 y (take (x-1)*8 grid ++ take 8 (cycle [-2]) ++ drop (x+1)*8 grid))

applyFPA2 :: Int -> [Int] -> [Int]
applyFPA2 y grid
    | y <= 64   = 
        let (left, right) = splitAt y grid
        in applyFPA2 y+8 (left ++ [-2] ++ (tail right))
    | otherwise = grid

evalObject :: String -> Int -> AObject
evalObject xTxt pNum = 
    let Txt = stripEOL xTxt
    in case (length Txt) of 5 -> 
        let tell = 
                            7 -> 
    | length (stripEOL xTxt) == 5 = 
        let tell = pairProcessor xTxt
            xMap = MapObject  pNum
        in case (row xMap, column xMap) of  ((-127),_) -> ErObject "undefinedMT"
                                            (_,(-127)) -> ErObject "undefinedMT"
                                            _ -> xMap
    | length (stripEOL xTxt) == 7 = ErObject "undefined"
    | otherwise = ErObject "inFault"



data Task = Task Lexeme

instance Show Task where
    show (Task (Ident x)) = show x

instance Read Task where
    readsPrec _ input = 
        let xk = tParse input
            isComb = elem ',' input
        in case (xk, isComb) of     (-1,False)  -> [(Task (Ident "nTask"), "")]
                                    (xk,True)   -> [(Task (Ident "InFault"), "")]
                                    (xk,False)  -> [(Task (Ident input), "")]