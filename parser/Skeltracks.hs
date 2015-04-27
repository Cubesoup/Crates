module Skeltracks where

-- Haskell module generated by the BNF converter

import Abstracks
import MusicTypes       

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

transTrackBlock :: TrackBlock -> Track
transTrackBlock x = case x of
  PTrack str1 str2 strs3 n4 n5 ns6 str7
    -> Track { title = str1
             , location = str2
             , artists = strs3
             , bpm = n4
             , duration = n5
             , cues = ns6
             , genre = str7 }


transReleaseBlock :: ReleaseBlock -> Release
transReleaseBlock x = case x of
  PRelease str1 n2 str3 strs4
    -> Release { name = str1
               , year = n2
               , label = str3
               , tracks = strs4 }


transCrateBlock :: CrateBlock -> Crate
transCrateBlock x = case x of
  PCrate str trackblocks releaseblocks
    -> undefined



