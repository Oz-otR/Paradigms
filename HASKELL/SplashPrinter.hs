module SplashPrinter (
    errorPrintOut
,   resultPrintOut
) where

resultPrintOut :: [Char] -> [Char] -> IO ()
resultPrintOut out results = do
    writeFile out results

errorPrintOut :: [Char] -> [Char] -> IO ()
errorPrintOut out errorText = do
    writeFile out errorText