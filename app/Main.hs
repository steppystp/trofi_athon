module Main where

import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Control.Monad.Trans.Writer (WriterT, execWriterT, runWriterT, tell)
import Data.List
import Helper (MaybeT, liftMaybeT, maybeReadInt, prompt, runMaybeT)
import Module.Item
import Module.Item (LogItem (UnknownItem), addNewItem, description, itemId, dateInput, itemCategory, parseItem, parseLogItem, agentCaring, deleteItem)
import Module.Message (LogMessage, makeLogMessage, parseLogMessage)
import System.IO (hFlush, stdout)

runProgram :: [LogItem] -> [LogMessage] -> IO ()
runProgram items messages = do
    putStrLn "\n\n\n=============== Transaction Record for IndiHome ==============="
    putStrLn $ replicate 63 '='
    putStrLn "(a) Show all Record (b) Add new record  (c) Remove record  (d) Exit program"
    choice <- prompt "Input choice: "
    case choice of
        "a" -> do
            putStrLn $ showAllItem items
            empty <- prompt "Press enter to go back"
            runProgram items messages
        "b" -> do
            putStrLn "\nYou're about to add new record, please fill the information below: "
            date <- prompt "Date Input (DDMMYYYY): "
            category <- prompt "Transaction name (PSB/MO/Ggn/Info/CAPS):"
            hFlush stdout
            agent <- prompt "Agent Caring: "
            description <- prompt "Description:"
            newItems <- addNewItem items date category agent description
            parseLogItem newItems
            logMessage <- makeLogMessage (last newItems) "NEW"
            parseLogMessage logMessage
            emptyPrompt <- prompt "Successfully added new record! Press enter to continue."
            runProgram newItems messages
        "c" -> do
            putStrLn "You're about to delete the record: "
            -- Insert ItemID
            putStr "Insert ItemID: "
            hFlush stdout
            choice <- do
                result <- runMaybeT maybeReadInt
                case result of
                    (Just a) -> return a
                    Nothing -> return 0

            updatedItems <- deleteItem items choice
            parseLogItem updatedItems

            let changedItem = find (\item -> itemId item == choice) updatedItems
                extractItem :: Maybe LogItem -> LogItem
                extractItem (Just a) = a
                extractItem Nothing = UnknownItem

            let extractedItem = extractItem changedItem

            logMessage <-
                if extractedItem == UnknownItem
                    then makeLogMessage extractedItem "ERR"
                    else makeLogMessage extractedItem "DEL"
            parseLogMessage logMessage
            emptyPrompt <- prompt "Press enter to continue."
            runProgram updatedItems messages
        "d" -> do
            putStrLn "Exiting program..."
            putStrLn "Thank you and Goodbye!"
        _ -> do
            empty <- prompt "Wrong input! Press enter to try again."
            runProgram items messages

showAllItem :: [LogItem] -> String
showAllItem [] = replicate 58 '='
showAllItem (item : rest) =
    "ID: " ++ show (itemId item)
        ++ "\nDate: "
        ++ dateInput item
        ++ "\nTrans: "
        ++ itemCategory item
        ++ "\nAgent Caring: "
        ++ agentCaring item
        ++ "\nDescription: "
        ++ description item
        ++ "\n"
        ++ replicate 29 '-'
        ++ "\n"
        ++ showAllItem rest

main :: IO ()
main = do
    items <- fmap parseItem (readFile "log/items.txt")
    runProgram items []