
module PropLang.Value(
    Value(..), newValueIORef
    ) where

import PropLang.Event
import Data.IORef


data Value a = Value {valSet :: a -> IO (), valGet :: IO a}

newValueIORef :: Event -> a -> IO (Value a)
newValueIORef e x = do
        i <- newIORef x
        return $ Value (setter i) (readIORef i)
    where
        setter i x = writeIORef i x >> raise e

