{-# LANGUAGE ExistentialQuantification #-}
module Screen where

import Graphics.Vty
import qualified Data.Map as Map
import Rendering -- for the help screen        

-- Run the first screen like this:
--       
--main = do
--     config <- standardIOConfig
--     vty <- mkVty config
--     runScreen (splashScreen :: Screen ()) vty ()
--     shutdown vty
       
--------------------------------
-- Screen Datatype and Driver --
--------------------------------
   
-- Each screen has a different state type and consists of a renderer, which
-- displays the state, and an event map, describing how to respond to key
-- events.   
       
data Screen a = forall b . Screen { renderer :: a -> Picture -- worry about resizing after
                                  , eventMap :: Vty -> Map.Map Event (Transition a b) }

-- A response to a key event can be to terminate the current screen,
-- update the state, or call a subscreen

data Transition a b = Terminate
                    | Update (a -> IO a)
                    | forall b . Transition { subScreen :: Screen b
                                           , before    :: a -> IO b
                                           , after     :: a -> b -> IO a }


-- This is the "Driver" (it "drives" a screen in the sense that it glues it all together)
runScreen :: forall a b . Screen a -> Vty -> a -> IO a
runScreen scr@Screen{ eventMap = evMap } vty st = do
    -- render the state
    update vty $ (renderer scr) st 

    -- handle terminal events according to screen event map
    ev <- nextEvent vty
    case Map.lookup ev (evMap vty) of
      Nothing -> runScreen scr vty st 
      Just r  -> case r of
        -- if our event is a termination event 
        Terminate -> return st
        -- if our event is a state update
        Update f  -> do updated <- f st
                        runScreen scr vty updated
        -- if out event is a subscreen transition
        t@Transition{ before = bef , after = aft , subScreen = subscr }
                  -> do prepared <- bef st                   -- state processed for use with subscreen
                        new <- runScreen subscr vty prepared -- drop down into subscreen
                        prepped <- aft st new                -- output of subscreen, processed for reuse in this screen
                        runScreen scr vty prepped            -- run current screen with new state

---------------------------------------------------    
-- Useful Functions For Programming With Screens --
---------------------------------------------------    
   
-- for defining key events, apply f is the update that does f to the state    
apply :: (a -> a) -> (a -> IO a)
apply f = return . f
       
-- for defining event maps from a list of (Vty.Event , Transition a b) pairs (better type errors)
toMap :: [(Event,Transition a b)] -> Map.Map Event (Transition a b)
toMap = Map.fromList

-----------------------------
-- The Help Screen Creator --
-----------------------------

-- creates a "help screen" from an image. Exit the help screen with '?'        
        
displayHelp :: Image -> Transition a ()
displayHelp helpImage = Transition { before = (\_ -> return ())   
                                   , after  = (\a () -> return a) -- upon returning, state is unchanged
                                   , subScreen = helpScreen helpImage }
        
helpScreen :: Image -> Screen ()
helpScreen helpimg = Screen { renderer = (\_ -> picForImage helpimg)
                          , eventMap = helpScreenMap }

-- default quit key is '?', could probably parameterize but let's sit on it.           
helpScreenMap vty = toMap [ (chEvent '?' , Terminate)]             
      
