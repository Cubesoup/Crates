{-# LANGUAGE FlexibleInstances #-}
module Rendering where

import Graphics.Vty
import MusicTypes
       
-------------------------  
-- Defining Key Events --
-------------------------
   
chEvent :: Char -> Event
chEvent x = EvKey (KChar x) []

backspace :: Event
backspace = EvKey KBS []          
        
ctrl :: Event -> Event
ctrl (EvKey k mods) = EvKey k (MCtrl : mods)

meta :: Event -> Event
meta (EvKey k mods) = EvKey k (MMeta : mods)

alt :: Event -> Event
alt (EvKey k mods) = EvKey k (MAlt : mods)

shift :: Event -> Event
shift (EvKey k mods) = EvKey k (MShift : mods)      

--------------------      
-- Drawing Images --
--------------------
   
class Img a where
  img :: a -> Image      
  
instance Img String where
  img = string defAttr       

instance Img Char where
  img = char defAttr     

instance Img Int where
  img = img . show         


-- probably better to do [Track] -> Image, for filter searches.  
trackList :: Int -> Int -> Release -> Image
trackList height width x = resize width height $ vlist (map listing (tracks x))

                             

-- argh which order? will presently break on empty artist lists
listing :: Track -> String
listing x = (foldr1 (\x y -> x ++ " & " ++ y) (artists x)) ++ " - " ++ (title x)
          
-- do we really ever want to pad the height? Could get rid of a parameter        
releaseBox :: Int -> Int -> Release -> Image
releaseBox height width x = box $ resize width height $ 
                              img (name x) <-> img (label x) <-> img (show (year x))
                              
-- could easily change to resize height
trackBox :: Int -> Track -> Image
trackBox width x = box $ resizeWidth width $
                    img (title x) <-> hlist (artists x) <->
                    (img (bpm x) <|> img (" BPM")) <->
                    img (showMinutes (duration x))
          
showMinutes :: Int -> String
showMinutes x = (show (x `div` 60)) ++ ":" ++ (show (x `mod` 60))

hlist :: [String] -> Image
hlist [] = emptyImage      
hlist xs = img $ foldr1 (\x y -> x ++ "," ++ y) xs
  
vlist :: [String] -> Image
vlist [] = emptyImage      
vlist xs = foldr1 (<->) $ map img xs     
  
box :: Image -> Image
box stuff = top <-> (vBar <|> stuff <|> vBar) <-> bottom
    where width  = imageWidth stuff
          height = imageHeight stuff
          hBar   = (img $ take width (repeat boxHoriz))
          vBar   = (vertCat $ map img $ take height (repeat boxVert))
          top    = (img boxUpLeft) <|> hBar <|> (img boxUpRight)
          bottom = (img boxDownLeft) <|> hBar <|> (img boxDownRight)

boxHoriz :: Char
boxHoriz = '─'

boxVert :: Char
boxVert = '│'

boxUpLeft :: Char
boxUpLeft = '┌'

boxUpRight :: Char
boxUpRight = '┐'

boxDownLeft :: Char
boxDownLeft = '└'

boxDownRight :: Char
boxDownRight = '┘'
       
