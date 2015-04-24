-- representing music bookkeeping data structures.
module MusicTypes where

import Text.Parsec hiding (label) -- we use 'label', use <?> instead
       
----------------------
-- Type Definitions --
----------------------       
   
data Track = Track { artists  :: [String]
                   , title    :: String
                   , location :: FilePath
                   , bpm      :: Int   -- to simplify things. Set to whatever if no bpm info available.
                   , duration :: Int   -- in seconds
                   , cues     :: [Int] }

data Release = Release { tracks :: [Track] -- in track listing order
                       , name   :: String
                       , year   :: Int
                       , label  :: String }

-------------     
-- Parsing --
-------------   

-- '"' is a forbidden character. Could add escape sequence for it.
string :: Stream 
string = do
  char '"'
  many (noneOf ['"'])
  char '"'

--------------
-- Examples --
--------------   

t1 :: Track
t1 = Track { artists = ["Ceph","Gammer"]
           , title = "This Doesn't Exist"
           , location = "/home/chad/music/t1.mp3"
           , bpm = 175
           , duration = 322
           , cues = []}
t2 = Track { artists = ["Serum"]
           , title = "Wub Wub Motherfucker"
           , location = "/home/chad/music/t2.flac"
           , bpm = 174
           , duration = 381
           , cues = []}
t3 = Track { artists = ["Tyl3R"]
           , title = "Bass Face"
           , location = "/home/chad/music/t3.mp3"
           , bpm = 178
           , duration = 206
           , cues = []}   


r1 :: Release
r1 = Release { tracks = [t1,t2,t3]
             , name   = "Best Album Evar"
             , year   = 2015
             , label  = "Bitchin Recordings" }


                     
     
        
