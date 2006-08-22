
module PropLang.Variable(
    Var, newVar, newVarName, newVarWith, newVarWithName,
    getVar,
    with, with1, with2, with3,
    (=<), (=$=), (=$$=),
    (-<), (-<-), (=<=), (=<>=),
    tie
    ) where

import PropLang.Event
import PropLang.Value
import Data.IORef
import Monad

infixr 1  -<, =<=, -<-, =<>=
infixl 8 =$=, =$$=
infixl 7 =<




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

with1 :: Var a -> (a -> x) -> ([Event], IO x)
with1 varFrom1 f = f =$$= varFrom1

with2 :: Var a -> Var a1 -> (a -> a1 -> x) -> ([Event], IO x)
with2 var1 var2 f = f =$$= var1 =$= var2

with3 :: Var a -> Var a1 -> Var a2 -> (a -> a1 -> a2 -> x) -> ([Event], IO x)
with3 var1 var2 var3 f = f =$$= var1 =$= var2 =$= var3

(=<) :: Var x -> ([Event], IO x) -> IO ()
(Var valTo _ source) =< (events,f) = do
    srcOld <- readIORef source
    mapM_ remove srcOld
    es <- mapM (+= g) events
    writeIORef source es
    g
    where
        g = f >>= valSet valTo

-- First parameter
(=$$=) :: (a->x) -> Var a -> ([Event], IO x)
f =$$= x = ([], return f) =$= x

-- Other parameters
(=$=) :: ([Event], IO (a -> x)) -> Var a -> ([Event], IO x)
(e,f) =$= v = (event v : e, f `ap` (getVar v))

