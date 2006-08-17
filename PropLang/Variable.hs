
module PropLang.Variable(
    Var, newVar, newVarName, newVarWith, newVarWithName,
    getVar,
    with, with1, with2, with3,
    (-<), (-<-), (=<), (=<=), (=<>=),
    tie
    ) where

import PropLang.Event
import PropLang.Value
import Data.IORef
import Monad

infixr 1  =<, -<, =<=, -<-, =<>=



data Var a = Var (Value a) Event (IORef [EventHandle])


newVar :: (Eq a) => a -> IO (Var a)
newVar x = newVarName "" x

newVarName :: (Eq a) => String -> a -> IO (Var a)
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
getVar (Var val _ _) = valGet val


instance Eventer (Var a) where
    event (Var _ evt _) = evt


(-<) :: Var a -> a -> IO ()
(-<) (Var val _ source) x = do
    writeIORef source []
    valSet val x

(-<-) :: Var a -> Var a -> IO ()
(-<-) varto varfrom = getVar varfrom >>= (varto -<)

(=<=) :: Var a -> Var a -> IO ()
var1 =<= var2 = var1 =< with1 var2 id



tie :: Var a -> Var b -> (a->b) -> (b->a) ->  IO ()
tie var1@(Var val1 _ _) var2@(Var val2 _ _) f12 f21 = do
        -- I tried something sophisticated to avoid loops, but 
	-- it did not work out. This will of course only work if 
	-- the Variables don't fire if they are set to what they are
	-- set already...
	-- Note that I'm not using =<=, not not override the sources
	var1 += (getVar var1 >>= (valSet val2).f12)
	var2 += (getVar var2 >>= (valSet val1).f21)
	return ()

        
(=<>=) :: Var a -> Var a -> IO ()
var1 =<>= var2 = tie var1 var2 id id

with = with1

with1 :: Var a -> (a -> x) -> Var x -> IO ()
with1 varFrom1 f var =
        withN [event varFrom1] g var
    where
        g = liftM f $ getVar varFrom1

with2 :: Var a -> Var b -> (a -> b -> x) -> Var x -> IO ()
with2 varFrom1 varFrom2 f var =
        withN [event varFrom1, event varFrom2] g var
    where
        g = liftM2 f (getVar varFrom1) (getVar varFrom2) 

with3 :: Var a -> Var b -> Var c -> (a -> b -> c -> x) -> Var x -> IO ()
with3 varFrom1 varFrom2 varFrom3 f var =
        withN [event varFrom1, event varFrom2, event varFrom3] g var
    where
        g = liftM3 f (getVar varFrom1) (getVar varFrom2) (getVar varFrom3)


withN :: [Event] -> IO x -> Var x -> IO ()
withN events f (Var valTo _ source) = do
    srcOld <- readIORef source
    mapM_ remove srcOld
    es <- mapM (+= g) events
    writeIORef source es
    g
    where
        g = f >>= valSet valTo


(=<) :: Var a -> (Var a -> IO ()) -> IO ()
var =< f   = f var

