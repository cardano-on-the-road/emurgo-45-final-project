module Logger where

import              Data.Text
import              Data.Time                                           (getCurrentTime)
import              Types
import              Control.Monad.Trans.Class                           (lift)


logEvent:: String -> IO Text
logEvent log = do
    time <- getCurrentTime
    let logLine = append (append (pack $ show time) (pack " - ")) (append (pack log) (pack "\n"))
    return logLine

logEvent':: String -> RWSIO Text
logEvent' log = do
    time <- lift getCurrentTime
    let logLine = append (append (pack $ show time) (pack " - ")) (append (pack log) (pack "\n"))
    return logLine