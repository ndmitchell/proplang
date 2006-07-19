
module PropLang.Variable(
    Var, newVar, newVarName, newVarWith, newVarWithName,
    getVar,
    with, with1, (-<), (=<), (=<=)
    ) where

import PropLang.Event
import PropLang.Value
import Data.IORef

infixr 1  =<=, =<, -<



data Var a = Var (Value a) Event (IORef (Maybe EventHandle))


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
    i <- newIORef Nothing
    return $ Var v e i


getVar :: Var a -> IO a
getVar (Var a b c) = valGet a


instance Eventer (Var a) where
    event (Var a b c) = b


data Action a b = Action (Var a) (a -> b)

with1 :: Var a -> (a -> b) -> Action a b
with1 var f = Action var f

with :: Var a -> Action a a
with var = with1 var id


(-<) :: Var a -> a -> IO ()
(-<) (Var a b c) val = (valSet a) val

(=<=) :: Var a -> Var a -> IO ()
(=<=) to from = to =< with from

(=<) :: Var a -> Action b a -> IO ()
(=<) (Var valTo _ source) (Action varFrom  f) = do
    srcOld <- readIORef source
    case srcOld of
        Nothing -> return ()
        Just x -> remove x
    note <- varFrom += g
    writeIORef source (Just note)
    g
    where
        g = do v <- getVar varFrom
               (valSet valTo) (f v)
