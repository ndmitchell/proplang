-----------------------------------------------------------------------------
-- |
-- Module      :  Event.hs
-- Copyright   :  (c) Neil Mitchell 2007
-- License     :
--
-- Maintainer  :
-- Stability   :  unstable
-- Portability :  not portable
--
-- PropLang events
--
-----------------------------------------------------------------------------

module PropLang.Event(
    Eventer(..), Event, newEvent, newEventName,
    EventHandle, (+=), remove,
    raise,
    blockEvent, unblockEvent
    ) where

import Control.Monad
import Data.IORef

debug = putStrLn

-- |
class Eventer a where
    event :: a -> Event

instance Eventer Event where
    event = id


-- | Event type. 
data Event = Event
                String --  Event Name
		(IORef Bool) --  Enable flag
                (IORef Integer) --  Next action index
                (IORef [(Integer, IO ())]) --  List of indices and actions

-- | EventHandle type.
--   Stores an Event and action index.
data EventHandle = EventHandle Event Integer


-- | Create a new Event.
newEvent :: IO Event
newEvent = newEventName ""

-- | Create a new Event with a name.
newEventName :: String -> IO Event
newEventName name = do
    debug $ "Creating event: " ++ name
    a <- newIORef True
    n <- newIORef 0
    xs <- newIORef []
    return $ Event name a n xs


-- | Attach an action to an Event.
(+=) :: Eventer a => a -> IO () -> IO EventHandle
(+=) e x = do
    let Event name a n xs = event e
    n2 <- readIORef n
    writeIORef n (n2+1)
    xs2 <- readIORef xs
    writeIORef xs ((n2,x) : xs2)
    debug $ "Added to event, now at " ++ show (length xs2+1) ++ ": " ++ name
    return $ EventHandle (event e) n2

-- | Remove an action from an Event.
remove :: EventHandle -> IO ()
remove (EventHandle e x) = do
    let Event name a n xs = event e
    xs2 <- readIORef xs
    writeIORef xs [(a,b) | (a,b) <- xs2, a /= x]
    debug $ "Removed from event, now at " ++ show (length xs2-1) ++ ": " ++ name


-- | Raise an Event.
raise :: Eventer a => a -> IO ()
raise e = do
    let Event name a n xs = event e
    active <- readIORef a
    if active then do
      xs2 <- readIORef xs
      debug $ "Raising event, " ++ show (length xs2) ++ ": " ++ name
      mapM_ snd xs2
     else 
      debug $ "Not rasing disabled event: " ++ name


-- | Block an Event.
blockEvent :: Event -> IO ()
blockEvent (Event _ a _ _) = writeIORef a False

-- | Unblock an Event.
unblockEvent :: Event -> IO ()
unblockEvent (Event _ a _ _) = writeIORef a True
