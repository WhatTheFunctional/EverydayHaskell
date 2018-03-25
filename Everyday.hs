--Everyday.hs
--Copyright Laurence Emms 2018
--Everyday Haskell example program

import System.IO --Required for hFlush, withFile
import System.Environment --Requirment for getArgs
import Data.List --Requirement for intercalate

orderType :: String -> IO ()
orderType menuItem
    | menuItem == "Ice cream" || menuItem == "Gulab Jamun" = putStr "Dessert ordered"
    | menuItem == "Pakora" || menuItem == "Mini samosas" = putStr "Starter ordered"
    | otherwise = return ()

printIsVIP :: Int -> IO ()
printIsVIP rewardPoints
    = if rewardPoints > 50
      then putStr "VIP member"
      else return ()

computeCustomerDiscount :: Int -> Int -> Float
computeCustomerDiscount price rewardPoints
    | rewardPoints > 5 = (fromIntegral price) * discountRate * tax
    | otherwise = (fromIntegral price) * tax
        where discountRate = 0.15
              tax = 0.2

printCustomerRecord :: (String, String, Int) -> IO ()
printCustomerRecord (name, menuItem, rewardPoints)
    = let stringRecord = show (name, menuItem, rewardPoints) in
          putStr stringRecord

findItemCost :: String -> [(String, Float)] -> Maybe Float
findItemCost _ [] = Nothing
findItemCost itemName ((item, cost) : menuItems)
    | itemName == item = Just cost
    | otherwise = findItemCost itemName menuItems

getTotalCustomerSpent :: [(String, String, Int)] -> [(String, Float)] -> Float
getTotalCustomerSpent [] _ = 0.0
getTotalCustomerSpent ((name, menuItem, rewardPoints) : records) menuItems
    = let menuItemCost = (findItemCost menuItem menuItems) in
          case menuItemCost of
          Nothing -> getTotalCustomerSpent records menuItems
          Just cost -> cost + (getTotalCustomerSpent records menuItems)
              
printCustomerMenuItems :: String -> [(String, String, Int)] -> [String] -> IO ()
printCustomerMenuItems customerName [] stack = putStr (intercalate " " stack) --The final record was processed
printCustomerMenuItems customerName ((name, menuItem, _) : records) stack
    | customerName == name = printCustomerMenuItems customerName records (menuItem : stack) --Push menu item on stack
    | otherwise = printCustomerMenuItems customerName records stack

--generateRecord :: IO (String, String, Int)
--generateRecord = putStr "Enter customer name:\n" >>
                 --hFlush stdout >>
                 --getLine >>= (\name ->
                 --putStr "Enter menu item:\n" >>
                 --hFlush stdout >>
                 --getLine >>= (\menuItem ->
                 --putStr "Enter number of reward points:\n" >>
                 --hFlush stdout >>
                 --readLn >>= (\rewardPoints -> return (name, menuItem, rewardPoints))))

generateRecord :: IO (String, String, Int)
generateRecord = do putStr "Enter customer name:\n"
                    hFlush stdout
                    name <- getLine
                    putStr "Enter menu item:\n"
                    hFlush stdout
                    menuItem <- getLine
                    putStr "Enter number of reward points:\n"
                    hFlush stdout
                    rewardPoints <- readLn
                    return (name, menuItem, rewardPoints)

prettyPrintRecord :: (String, String, Int) -> IO ()
prettyPrintRecord (name, menuItem, rewardPoints) = putStr "Customer: " >>
                                                   putStr name >>
                                                   putStr ", " >>
                                                   putStr menuItem >>
                                                   putStr ", " >>
                                                   print rewardPoints >>
                                                   putStr "\n"

printRecordFileName :: Either String String -> IO ()
printRecordFileName (Left err) = putStrLn err
printRecordFileName (Right fileName) = putStr "Record file: " >>
                                       putStrLn fileName

getFileName :: [String] -> Either String String
getFileName [] = Left "Failed to read file name."
getFileName (fileName : args) = Right fileName

main = getArgs >>= (\args ->
       printRecordFileName (getFileName args) >>
       generateRecord >>=
       prettyPrintRecord)
