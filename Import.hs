module Import where

import MusicTypes
import Screen
import Rendering
import Graphics.Vty       
import qualified Sound.TagLib as Tag
import Data.Convertible
import Data.List.Split (wordsBy)
import Data.List.PointedList       
import Data.List (intersperse)
import qualified Data.List.PointedList.Circular as C
       
-- Instead of passing around a crate, we parameterize the import screen by one.

-- two screens, importRelease, and
-- importTrack. importTrack is more
-- important.   

-- TODO: Implements editable fields, right now they're static

-- TODO: Make it actually add the track to the crate with C-s or something.    
   
-- TODO: Import Release Screen. 

-- we import a track based on a file path, so
setupTrackImport :: FilePath -> IO TrackImportState
setupTrackImport path = do
    t1 <- inferTagInfo path (emptyTrack path)
    t2 <- inferAudioProperties path t1
    return $ TIS { trackInfo = t2
                 , fields = generateFields t2 }

generateFields :: Track -> PointedList String
generateFields t = unsafeFromList
                     [ title t
                     , concat (intersperse " & " (artists t))
                     , genre t ]               

unsafeFromList :: [a] -> PointedList a
unsafeFromList = (\(Just x) -> x) . fromList               
    
-- expects a track with every field defined, filepath correct
importTrack :: Crate -> Screen TrackImportState
importTrack crate = Screen { renderer = renderTrackImportState
                           , eventMap = trackImportEventMap crate }

renderTrackImportState :: TrackImportState -> Picture
renderTrackImportState tis = picForImage $ 
                             img (location (trackInfo tis)) <->
                             renderFields (fields tis)

renderFields :: PointedList String -> Image
renderFields pl = box $ vertCat $ toList $ 
                    replace (boldBoxed (_focus pl)) (fmap toBoxed pl)
    where
      toBoxed :: String -> Image
      toBoxed s = box $ resize fieldWidth 1 $ img s
      boldBoxed :: String -> Image
      boldBoxed s = boldBox $ resize fieldWidth 1 $ img s    

toList :: PointedList a -> [a]
toList pl = reverse (_reversedPrefix pl) ++ [(_focus pl)] ++ (_suffix pl)
    
fieldWidth :: Int
fieldWidth = 20           
    
trackImportEventMap crate vty =
    toMap $ [ (ctrl (chEvent 'q') , Terminate)
            , (tab                , Update (apply focusDown))
            , (backtab            , Update (apply focusUp))
            ]

focusUp :: TrackImportState -> TrackImportState
focusUp tis = TIS { trackInfo = trackInfo tis
                  , fields = C.previous (fields tis) }

focusDown :: TrackImportState -> TrackImportState
focusDown tis = TIS { trackInfo = trackInfo tis
                    , fields = C.next (fields tis) }

data TrackImportState = TIS { trackInfo :: Track
                            , fields :: PointedList String }

-----------------------------------------------
-- Functions for inferring track information --
-----------------------------------------------   

inferTagInfo :: FilePath -> Track -> IO Track
inferTagInfo path track = do
    mtag <- getTagInfo path
    case mtag of
      Nothing -> return track
      Just t  -> do
        tagTitle <- Tag.title t
        tagArtist <- Tag.artist t
        tagGenre <- Tag.genre t
        tagYear <- Tag.year t
        tagAlbum <- Tag.album t
        tagComment <- Tag.comment t
        tagTrackNum <- Tag.track t
        return $ track { title = tagTitle
                       , artists = splitArtists tagArtist
                       , genre = tagGenre
                       }

splitArtists :: String -> [String]
splitArtists = wordsBy (== '&')
    
inferAudioProperties :: FilePath -> Track -> IO Track
inferAudioProperties path track = do
    maupro <- getAudioProperties path
    case maupro of
      Nothing -> return track
      Just t ->  do
        fBitRate <- Tag.bitRate t
        fDuration <- Tag.duration t
        fChannels <- Tag.channels t
        fSampleRate <- Tag.sampleRate t
        return $ track { duration = convert fDuration }
               

getAudioProperties :: FilePath -> IO (Maybe Tag.AudioProperties)
getAudioProperties path = do
    mtagfile <- Tag.open path
    case mtagfile of
      Nothing -> return Nothing
      Just tagfile -> do
          Tag.audioProperties tagfile
                               
getTagInfo :: FilePath -> IO (Maybe Tag.Tag)
getTagInfo path = do
    mtagfile <- Tag.open path
    case mtagfile of
      Nothing -> return Nothing
      Just tagfile -> do
          Tag.tag tagfile
      
             
                      
