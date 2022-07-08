module Module.Item where

import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Control.Monad.Trans.Writer (WriterT, execWriterT, runWriterT, tell)
import Data.List
import Helper (MaybeT, liftMaybeT, maybeReadInt, prompt, runMaybeT)
import System.IO (hFlush, stdout)

data LogItem
    = LogItem
        { itemId :: Int
        , dateInput :: String
        , itemCategory :: String
        , agentCaring :: String
        , description :: String
        }
    | UnknownItem
    deriving (Show, Eq)

addNewItem :: [LogItem] -> String -> String -> String -> String -> IO [LogItem]
addNewItem oldLogItemList date category agent description = do
    let lastId =
            if null oldLogItemList
                then 0
                else itemId $ last oldLogItemList
        newId = lastId + 1
        newLogItem =
            LogItem
                { itemId = newId
                , dateInput = date
                , itemCategory = category
                , agentCaring = agent
                , description = description
                }
    let newLogItemList = oldLogItemList ++ [newLogItem]
    return newLogItemList

deleteItem :: [LogItem] -> Int -> IO [LogItem]
deleteItem oldLogItemList choice  = do
    let itemExist = find (\item -> (itemId item) == choice) oldLogItemList

        extractItem :: Maybe LogItem -> LogItem
        extractItem (Just a) = a
        extractItem Nothing = UnknownItem

        removeItem :: [LogItem] -> LogItem -> [LogItem]
        removeItem [] chosenItem  = []
        removeItem (item : rest) chosenItem 
            | item == chosenItem = removeItem rest chosenItem 
            | otherwise = [item] ++ removeItem rest chosenItem 

    let updatedLogItemList =
            if (extractItem itemExist) == UnknownItem
                then oldLogItemList
                else removeItem oldLogItemList (extractItem itemExist)

    if (extractItem itemExist) == UnknownItem
        then putStrLn "Item not found. Please check your ItemID"
        else putStrLn "Successfully remove the record!"

    return updatedLogItemList

parseLogItem :: [LogItem] -> IO ()
parseLogItem logItemList = do
    let convertToLog :: [LogItem] -> String
        convertToLog [] = ""
        convertToLog (logItem : rest) =
            show (itemId logItem)
                ++ " "
                ++ dateInput logItem
                ++ " "
                ++ itemCategory logItem
                ++ " "
                ++ agentCaring logItem
                ++ " "
                ++ description logItem
                ++ "\n"
                ++ convertToLog rest
    let parsedLogItem = init $ convertToLog logItemList -- using init to remove the last \n at the end of the .log
    writeFile "log/items.txt" parsedLogItem

parseItem :: String -> [LogItem]
parseItem rawContent = map parseSingleItem (lines rawContent)

parseSingleItem :: String -> LogItem
parseSingleItem str = case words str of
    (i : t : n : m : d) -> makeItem i t n m  d
    _ -> UnknownItem

makeItem :: String -> String -> String  -> String -> [String] -> LogItem
makeItem itemId dateInput itemCategory agentCaring description =
    LogItem
        { itemId = read itemId
        , dateInput = dateInput
        , itemCategory = itemCategory
        , agentCaring = agentCaring
        , description = unwords description
        }