--Everyday.hs
--Copyright Laurence Emms 2018
--Everyday Haskell example program

import System.IO --Required for hFlush, withFile
import System.Environment --Requirment for getArgs

generateRecord :: IO (String, String, Int)
generateRecord = putStr "Enter customer name:\n" >>
                 hFlush stdout >>
                 getLine >>= (\name ->
                 putStr "Enter menu item:\n" >>
                 hFlush stdout >>
                 getLine >>= (\menuItem ->
                 putStr "Enter number of reward points:\n" >>
                 hFlush stdout >>
                 readLn >>= (\rewardPoints -> return (name, menuItem, rewardPoints))))

printRecord :: (String, String, Int) -> IO ()
printRecord (name, menuItem, rewardPoints) = putStr "Customer: " >>
                                             putStr name >>
                                             putStr ", " >>
                                             putStr menuItem >>
                                             putStr ", " >>
                                             print rewardPoints >>
                                             putStr "\n"

printRecordFileName :: [String] -> IO ()
printRecordFileName args = putStr "Record file: " >>
                           putStrLn (head args)

printRecordFileContents :: String -> IO ()
printRecordFileContents filePath
    = withFile filePath ReadMode (\handle ->
          hGetContents handle >>= (\contents ->
          putStrLn contents))

main = getArgs >>= (\args ->
       printRecordFileName args >>
       printRecordFileContents (head args) >>
       generateRecord >>=
       printRecord)
