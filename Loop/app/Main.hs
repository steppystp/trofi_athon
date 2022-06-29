module Main where

import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Control.Monad.Trans.Writer (WriterT, execWriterT, runWriterT, tell)
import Data.List
import Helper (MaybeT, liftMaybeT, maybeReadInt, prompt, runMaybeT)
import Module.Item (LogItem (UnknownItem), addNewItem, description, itemId, itemName, parseItem, parseLogItem, restockItem, storage, takeItem)
import Module.Message (LogMessage, makeLogMessage, parseLogMessage)
import System.IO (hFlush, stdout)

runProgram :: [LogItem] -> [LogMessage] -> IO ()
runProgram items messages = do
    putStrLn "\n\n\n=============== Loyalty Point Exchange ==============="
    ---putStrLn $ replicate 58 '='
    ---putStrLn $ showItem items
    putStrLn "\n\nApa yang ingin anda kerjakan?" 
    putStrLn "(a) Info Poin  (b) Konversi Poin  (e) Exit program" 
    choice <- prompt "Masukkan pilihan : " 
    case choice of
        "a" -> do
            --putStrLn $ showAllItem items
            noIndihome <-prompt "\nMasukan No IndiHome Anda: "
            let poin = "100" 
            -- <- stdout
            
            if noIndihome == "123456789"
                then putStrLn ("\nNomor Indihome "++noIndihome++ " memiilki " ++poin++ "poin")
                --runProgram items messages

            else putStrLn "\nAnda tidak memiliki poin\n"
            empty <- prompt "Press enter to go back"
            runProgram items messages
        
        "b" -> do
            putStrLn "Kamu akan melakukan konversi poin Indihome menjadi poin Telkomsel"
            -- Insert ItemID
            noIndihome2 <- prompt "\n Masukkan No Indihome Anda : "
            noTsel <- prompt "\n Masukkan No Tsel Anda : "
            
            let nikIndihome = "12345"
            let nikTsel = "12345"
            let poin = "100"
            --newpoin :: int -> int
            --konversi :: String -> String

            --if nikIndihome == nikTsel
                --then putStrLn "\n Anda memiliki" ++poin++ "poin indihome"
                --putrStrLn "\n Apakah anda ingin mengkonversi poin Indihome?"
                --putrStrLn "(Y) Yes (N) No"
                --konversi <- prompt "Masukan pilihan Anda"
                --case konversi of
                        --"Yes" -> do
                           -- newpoin = poin/10
                            --putrStrLn "Anda berhasil menukarkan" ++poin++ "Poin Indihome Menjadi" ++newpoin++ " Poin Telkomsel"
                        --"No" -> do
                            --putrStrLn "Terimakasih, datang kembali"
            
           -- else putStrLn "\n No Indihome dan Telkomsel tidak terdaftar pada NIk yang sama"
            empty <- prompt "Press enter to go back"
            runProgram items messages

            
            choice <- do
                result <- runMaybeT maybeReadInt
                case result of
                    (Just a) -> return a
                    Nothing -> return 0
            -- Insert Amount
            putStr "Insert amount to restock: "
            hFlush stdout
            amount <- do
                result <- runMaybeT maybeReadInt
                case result of
                    (Just a) -> return a
                    Nothing -> return 0

            newRestockedItems <- restockItem items choice amount
            parseLogItem newRestockedItems
            let changedItem = find (\item -> itemId item == choice) newRestockedItems
                extractItem :: Maybe LogItem -> LogItem
                extractItem (Just a) = a
                extractItem Nothing = UnknownItem

            let extractedItem = extractItem changedItem

            logMessage <-
                if extractedItem == UnknownItem
                    then makeLogMessage extractedItem "ERR"
                    else makeLogMessage (extractedItem{storage = amount}) "IN"

            parseLogMessage logMessage
            emptyPrompt <- prompt "Press enter to continue."
            runProgram newRestockedItems messages
            putStrLn "You're about to restock some item: "
            -- Insert ItemID
            putStr "Insert ItemID: "
            hFlush stdout
            choice <- do
                result <- runMaybeT maybeReadInt
                case result of
                    (Just a) -> return a
                    Nothing -> return 0
            -- Insert Amount
            putStr "Insert amount to restock: "
            hFlush stdout
            amount <- do
                result <- runMaybeT maybeReadInt
                case result of
                    (Just a) -> return a
                    Nothing -> return 0

            newRestockedItems <- restockItem items choice amount
            parseLogItem newRestockedItems
            let changedItem = find (\item -> itemId item == choice) newRestockedItems
                extractItem :: Maybe LogItem -> LogItem
                extractItem (Just a) = a
                extractItem Nothing = UnknownItem

            let extractedItem = extractItem changedItem

            logMessage <-
                if extractedItem == UnknownItem
                    then makeLogMessage extractedItem "ERR"
                    else makeLogMessage (extractedItem{storage = amount}) "IN"

            parseLogMessage logMessage
            emptyPrompt <- prompt "Press enter to continue."
            runProgram newRestockedItems messages
        "c" -> do
            putStrLn "You're about to take out some item: "
            -- Insert ItemID
            putStr "Insert ItemID: "
            hFlush stdout
            choice <- do
                result <- runMaybeT maybeReadInt
                case result of
                    (Just a) -> return a
                    Nothing -> return 0
            -- Insert Amount
            putStr "Insert amount to take: "
            hFlush stdout
            amount <- do
                result <- runMaybeT maybeReadInt
                case result of
                    (Just a) -> return a
                    Nothing -> return 0

            updatedItems <- takeItem items choice amount
            parseLogItem updatedItems

            let changedItem = find (\item -> itemId item == choice) updatedItems
                extractItem :: Maybe LogItem -> LogItem
                extractItem (Just a) = a
                extractItem Nothing = UnknownItem

            let extractedItem = extractItem changedItem

            logMessage <-
                if extractedItem == UnknownItem
                    then makeLogMessage extractedItem "ERR"
                    else
                        if amount > storage extractedItem
                            then makeLogMessage (extractedItem{storage = 0}) "ERR"
                            else makeLogMessage (extractedItem{storage = amount}) "OUT"
            parseLogMessage logMessage
            emptyPrompt <- prompt "Press enter to continue."
            runProgram updatedItems messages
        "d" -> do
            putStrLn "\nYou're about to add new item into the inventory, please fill the information below: "
            name <- prompt "Item name: "
            putStr "Quantity: "
            hFlush stdout
            storage <- do
                result <- runMaybeT maybeReadInt
                case result of
                    (Just a) -> return a
                    Nothing -> return 0
            description <- prompt "Description: "
            newItems <- addNewItem items name storage description
            parseLogItem newItems
            logMessage <- makeLogMessage (last newItems) "NEW"
            parseLogMessage logMessage
            emptyPrompt <- prompt "Successfully added new item! Press enter to continue."
            runProgram newItems messages
        "e" -> do
            putStrLn "Exiting program..."
            putStrLn "Goodbye!"
        _ -> do
            empty <- prompt "Wrong input! Press enter to try again."
            runProgram items messages

showItem :: [LogItem] -> String
showItem items = showItemFunc (length items) (take 2 items)
  where
    showItemFunc count [] = case count of
        0 -> "The item list is currently empty.\n" ++ replicate 58 '='
        1 -> "\n" ++ replicate 58 '='
        2 -> "\n" ++ replicate 58 '='
        _ -> "...and " ++ show (count - 2) ++ " more." ++ "\n" ++ replicate 58 '='
    showItemFunc count (item : rest) =
        "ID: " ++ show (itemId item)
            ++ "\nName: "
            ++ itemName item
            ++ "\nStorage: "
            ++ show (storage item)
            ++ "\nDescription: "
            ++ description item
            ++ "\n"
            ++ replicate 29 '-'
            ++ "\n"
            ++ showItemFunc count rest

showAllItem :: [LogItem] -> String
showAllItem [] = replicate 58 '='
showAllItem (item : rest) =
    "ID: " ++ show (itemId item)
        ++ "\nName: "
        ++ itemName item
        ++ "\nStorage: "
        ++ show (storage item)
        ++ "\nDescription: "
        ++ description item
        ++ "\n"
        ++ replicate 29 '-'
        ++ "\n"
        ++ showAllItem rest

main :: IO ()
main = do
    items <- fmap parseItem (readFile "log/items.log")
    runProgram items []