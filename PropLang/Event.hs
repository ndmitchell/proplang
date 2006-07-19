
module PropLang.Event(
    Eventer(..), Event, newEvent, newEventName,
    EventHandle, (+=), remove,
    raise
    ) where

import Data.IORef

debug = putStrLn

class Eventer a where
    event :: a -> Event

instance Eventer Event where
    event = id


data Event = Event
                String
                (IORef Integer)
                (IORef [(Integer, IO ())])

data EventHandle = EventHandle Event Integer


newEvent :: IO Event
newEvent = newEventName ""

newEventName :: String -> IO Event
newEventName name = do
    debug $ "Creating event: " ++ name
    n <- newIORef 0
    xs <- newIORef []
    return $ Event name n xs


(+=) :: Eventer a => a -> IO () -> IO EventHandle
(+=) e x = do
    let Event name n xs = event e
    n2 <- readIORef n
    writeIORef n (n2+1)
    xs2 <- readIORef xs
    writeIORef xs ((n2,x) : xs2)
    debug $ "Added to event, now at " ++ show (length xs2+1) ++ ": " ++ name
    return $ EventHandle (event e) n2

remove :: EventHandle -> IO ()
remove (EventHandle e x) = do
    let Event name n xs = event e
    xs2 <- readIORef xs
    writeIORef xs [(a,b) | (a,b) <- xs2, a /= x]
    debug $ "Removed from event, now at " ++ show (length xs2-1) ++ ": " ++ name


raise :: Eventer a => a -> IO ()
raise e = do
    let Event name n xs = event e
    xs2 <- readIORef xs
    debug $ "Raising event, " ++ show (length xs2) ++ ": " ++ name
    mapM_ snd xs2