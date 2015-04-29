module Import where

import MusicTypes
import Screen
import Rendering
import Graphics.Vty       
       
-- two screens, importRelease, and
-- importTrack. importTrack is more
-- important.   

-- TODO: everything -- might need more complex state
   
-- expects a track with every field defined, filepath correct
importTrack :: Screen Track
importTrack = Screen { renderer = renderTrackImport
                     , eventMap = trackImportEventMap }

renderTrackImport :: Track -> Picture
renderTrackImport = undefined                  

trackImportEventMap = undefined                  


type TrackImportState = Track

-- again, more state?     
importRelease :: Screen Release     
importRelease = Screen { renderer = renderReleaseImport
                       , eventMap = importReleaseEventMap }

renderReleaseImport :: Release -> Picture
renderReleaseImport = undefined

importReleaseEventMap = undefined                    
              
