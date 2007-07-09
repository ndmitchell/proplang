-----------------------------------------------------------------------------
-- |
-- Module      :  Variable.hs
-- Copyright   :  (c) Neil Mitchell 2007
-- License     :
--
-- Maintainer  :
-- Stability   :  unstable
-- Portability :  not portable
--
-- Defines PropLang variables with utility functions
--
-----------------------------------------------------------------------------

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
import Control.Monad
import Data.IORef

infixr 1  -<, =<=, -<-, =<>=
infixl 8 =$=, =$$=
infixl 7 =<



-- | A Variable, contains a value and event
data Var a = Var (Value a) Event (IORef [EventHandle])

-- | Create a new Variable
newVar :: (Eq a) => a -> IO (Var a)
newVar x = newVarName "" x

-- | Create a new Variable with an event name
newVarName :: (Eq a) => String -> a -> IO (Var a)
newVarName name x = newVarWithName name (`newValueIORef` x) 

-- | Create a new Variable with an action
newVarWith :: (Event -> IO (Value a)) -> IO (Var a)
newVarWith f = newVarWithName "" f

-- | Create a new Variable with a name and action
newVarWithName :: String -> (Event -> IO (Value a)) -> IO (Var a)
newVarWithName name f = do
    e <- newEventName name
    v <- f e
    i <- newIORef []
    return $ Var v e i


-- | Return the contents of a Variable
getVar :: Var a -> IO a
getVar (Var val _ _) = valGet val


instance Eventer (Var a) where
    event (Var _ evt _) = evt


-- | Inject a value into a Variable
(-<) :: Var a -> a -> IO ()
(-<) (Var val _ source) x = do
    writeIORef source []
    valSet val x

-- | Inject a value from a Variable into another
(-<-) :: Var a -> Var a -> IO ()
(-<-) varto varfrom = getVar varfrom >>= (varto -<)

-- | Tie another Variable with this one
(=<=) :: Var a -> Var a -> IO ()
var1 =<= var2 = var1 =< with1 var2 id


-- | Tie two Variables together with filter functions
tie :: Var a -> Var b -> (a->b) -> (b->a) ->  IO ()
tie var1@(Var val1 _ _) var2@(Var val2 _ _) f12 f21 = do
        -- I tried something sophisticated to avoid loops, but 
	-- it did not work out. This will of course only work if 
	-- the Variables don't fire if they are set to what they are
	-- set already...
	-- Note that I'm not using =<=, not not override the sources
	var1 += (valSet val2 .f12 =<< getVar var1 )
	var2 += (valSet val1 .f21 =<< getVar var2 )
	return ()

-- | Tie two Variables both ways 
(=<>=) :: Var a -> Var a -> IO ()
var1 =<>= var2 = tie var1 var2 id id

-- |
with :: Var a -> (a -> x) -> ([Event], IO x)
with = with1

-- | Run a function over a Variable
with1 :: Var a -> (a -> x) -> ([Event], IO x)
with1 varFrom1 f = f =$$= varFrom1

-- |
with2 :: Var a -> Var a1 -> (a -> a1 -> x) -> ([Event], IO x)
with2 var1 var2 f = f =$$= var1 =$= var2

-- |
with3 :: Var a -> Var a1 -> Var a2 -> (a -> a1 -> a2 -> x) -> ([Event], IO x)
with3 var1 var2 var3 f = f =$$= var1 =$= var2 =$= var3

-- | Tie this Variable with the results of a function
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

