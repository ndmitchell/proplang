-----------------------------------------------------------------------------
-- |
-- Module      :  Value.hs
-- Copyright   :  (c) Neil Mitchell 2007
-- License     :
--
-- Maintainer  :
-- Stability   :  unstable
-- Portability :  not portable
--
-- Defines PropLang values for use in Variables
--
-----------------------------------------------------------------------------

module PropLang.Value(
    Value(..), newValueIORef, newPredValue
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

newPredValue :: (Eq a) => a -> (a -> Bool) -> Event -> IO (Value a)
newPredValue x f e = do
	i <- newIORef x
	return $ Value (setter i) (readIORef i)
    where
	setter i x = do
	    old <- readIORef i
	    writeIORef i x
	    if f x
		then raise e
		else return ()
