
module PropLang.Variable(
    Var, newVar, newVarName, newVarWith, newVarWithName,
    getVar,
    with, with1, with2, with3,
    (-<), (=<), (=<=)
    ) where

import PropLang.Event
import PropLang.Value
import Data.IORef

infixr 1  =<, -<, =<=



data Var a = Var (Value a) Event (IORef [EventHandle])


newVar :: a -> IO (Var a)
newVar x = newVarName "" x

newVarName :: String -> a -> IO (Var a)
newVarName name x = newVarWithName name (`newValueIORef` x) 

newVarWith :: (Event -> IO (Value a)) -> IO (Var a)
newVarWith f = newVarWithName "" f

newVarWithName :: String -> (Event -> IO (Value a)) -> IO (Var a)
newVarWithName name f = do
    e <- newEventName name
    v <- f e
    i <- newIORef []
    return $ Var v e i


getVar :: Var a -> IO a
getVar (Var a b c) = valGet a


instance Eventer (Var a) where
    event (Var a b c) = b


(-<) :: Var a -> a -> IO ()
(-<) (Var val b source) x = do
    writeIORef source []
    (valSet val) x


(=<=) :: Var a -> Var a -> IO ()
a =<= b = a =< with1 b id


with = with1

with1 :: Var a -> (a -> x) -> Var x -> IO ()
with1 varFrom1 f var =
        withN [event varFrom1] g var
    where
        g = do v <- getVar varFrom1
               return $ f v

with2 :: Var a -> Var b -> (a -> b -> x) -> Var x -> IO ()
with2 varFrom1 varFrom2 f var =
        withN [event varFrom1, event varFrom2] g var
    where
        g = do v1 <- getVar varFrom1
               v2 <- getVar varFrom2
               return $ f v1 v2

with3 :: Var a -> Var b -> Var c -> (a -> b -> c -> x) -> Var x -> IO ()
with3 varFrom1 varFrom2 varFrom3 f var =
        withN [event varFrom1, event varFrom2, event varFrom3] g var
    where
        g = do v1 <- getVar varFrom1
               v2 <- getVar varFrom2
               v3 <- getVar varFrom3
               return $ f v1 v2 v3


withN :: [Event] -> IO x -> Var x -> IO ()
withN events f (Var valTo _ source) = do
    srcOld <- readIORef source
    mapM_ remove srcOld
    es <- mapM (+= g) events
    writeIORef source es
    g
    where
        g = do x <- f
               (valSet valTo) x


(=<) :: Var a -> (Var a -> IO ()) -> IO ()
(=<) var f = f var

