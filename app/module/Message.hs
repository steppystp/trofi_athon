module Module.Message where

import Data.Int
import Data.Time
import Data.Time.Clock.POSIX
import Module.Item

data LogMessage
    = LogMessage
        { item :: Int
        , category :: String
        , timestamp :: Int
        , status :: Status
        }
    | Unknown
    deriving (Show)

data Status = DEL | NEW | ERR deriving (Show, Read)

secondSinceEpoch :: UTCTime -> Int
secondSinceEpoch =
    floor . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds

makeLogMessage :: LogItem -> String -> IO LogMessage
makeLogMessage item status = do
    u <- getCurrentTime
    let currentTime = secondSinceEpoch u
        message =
            if item == UnknownItem
                then
                    LogMessage
                        { item = 0
                        , category = "no transaction"
                        , timestamp = currentTime
                        , status = ERR
                        }
                else
                    LogMessage
                        { item = itemId item
                        , category = itemCategory item
                        , timestamp = currentTime
                        , status = read status :: Status
                        }
    return message

parseLogMessage :: LogMessage -> IO ()
parseLogMessage message = do
    u <- getCurrentTime
    let currentTime = secondSinceEpoch u
    let parsedLogMessage =
            "ItemID: "
                ++ show (item message)
                ++ " | Status: "
                ++ show (status message)
                ++ " | Transaction: "
                ++ show (category message)
                ++ " | Timestamp: "
                ++ show (currentTime)
                ++ "\n"
    appendFile "log/messages.txt" parsedLogMessage
