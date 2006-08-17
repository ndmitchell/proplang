
module PropLang.Value(
    Value(..), newValueIORef
    ) where

import PropLang.Event
import Data.IORef


data Value a = Value {valSet :: a -> IO (), valGet :: IO a}

newValueIORef :: (Eq a) => Event -> a -> IO (Value a)
newValueIORef e x = do
        i <- newIORef x
        return $ Value (setter i) (readIORef i)
    where
        setter i x = do
	    old <- readIORef i
	    writeIORef i x
	    if old /= x then do raise e
	                else do return ()

