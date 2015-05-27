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

up :: Event
up = EvKey KUp []

down :: Event
down = EvKey KDown []     

kright :: Event
kright = EvKey KRight []

kleft :: Event
kleft = EvKey KLeft []

enter :: Event
enter = EvKey KEnter []

tab :: Event
tab = chEvent '\t'

backtab :: Event -- probably shift + tab
backtab = EvKey KBackTab []        
    
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


      
boxWith :: Char -> Char -> Char -> Char -> Char -> Char -> Image -> Image
boxWith upleft upright downleft downright vert horiz stuff =
          top <-> (vBar <|> stuff <|> vBar) <-> bottom
    where width  = imageWidth stuff
          height = imageHeight stuff
          hBar   = (img $ take width (repeat horiz))
          vBar   = (vertCat $ map img $ take height (repeat vert))
          top    = (img upleft) <|> hBar <|> (img upright)
          bottom = (img downleft) <|> hBar <|> (img downright)

box :: Image -> Image
box = boxWith boxUpLeft boxUpRight boxDownLeft boxDownRight boxVert boxHoriz

boldBox :: Image -> Image
boldBox = boxWith boldBoxUpLeft boldBoxUpRight boldBoxDownLeft boldBoxDownRight
                  boldBoxVert boldBoxHoriz

boxHoriz :: Char
boxHoriz = '─'

boldBoxHoriz :: Char
boldBoxHoriz = '━'
         
boxVert :: Char
boxVert = '│'

boldBoxVert :: Char
boldBoxVert = '┃'

boxUpLeft :: Char
boxUpLeft = '┌'

boldBoxUpLeft :: Char
boldBoxUpLeft = '┏'              

boxUpRight :: Char
boxUpRight = '┐'

boldBoxUpRight :: Char
boldBoxUpRight = '┓'

boxDownLeft :: Char
boxDownLeft = '└'

boldBoxDownLeft :: Char
boldBoxDownLeft = '┗'
            
boxDownRight :: Char
boxDownRight = '┘'

boldBoxDownRight :: Char
boldBoxDownRight = '┛'
       
