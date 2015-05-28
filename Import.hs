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

-- TODO: Make it actually add the track to the crate with C-s or something.    
   
-- TODO: Import Release Screen. 

-- we import a track based on a file path, so
setupTrackImport :: FilePath -> IO TrackImportState
setupTrackImport path = do
    t1 <- inferTagInfo path (emptyTrack path)
    t2 <- inferAudioProperties path t1
    return $ TIS { trackInfo = t2
                 , fields = generateFields t2 }

    
-- expects a track with every field defined, filepath correct
importTrack :: Crate -> Screen TrackImportState
importTrack crate = Screen { renderer = renderTrackImportState
                           , eventMap = trackImportEventMap crate }

    
trackImportEventMap crate vty =
    toMap $ [ (ctrl (chEvent 'q') , Terminate)
            , (tab                , Update (apply focusDown))
            , (backtab            , Update (apply focusUp))
            , (up                 , Update (apply focusUp))
            , (down               , Update (apply focusDown))
            , (kright             , Update (apply cursorRight))
            , (kleft              , Update (apply cursorLeft))
            , (backspace          , Update (apply deleteAtCursor))
            ] ++ charInsertEvents


deleteAtCursor :: TrackImportState -> TrackImportState
deleteAtCursor tis = TIS { trackInfo = trackInfo tis
                         , fields = applyToFocus
                                      deleteFocus
                                      (fields tis) }
    where
      deleteFocus :: CursorString -> CursorString
      deleteFocus pl | (atStart pl) && (atEnd pl) = pl
                     | (atStart pl) = (\(Just x) -> x) $ deleteRight pl
                     | otherwise =  (\(Just x) -> x) $ deleteLeft pl
    
-- events to edit focused string based on keypress. need valid characters for artist names,
-- album names, track names. Let's ignore the non-latin alphabets for now, because idfk how
-- to deal with, say, japanese. Notably, '?' is disallowed. (help page!), as are
-- '|' (pipes) and '/', '\' (directories). Also ';' because schenanigans
charInsertEvents :: [(Event, Transition TrackImportState b)]
charInsertEvents = map editUpdate fieldCharWhitelist

fieldCharWhitelist :: [Char]
fieldCharWhitelist = ['a'..'z'] ++ ['A'..'Z'] ++ ['1'..'0'] ++
                     ['*','!','@','#','$','%','^','&','(',')','\'','\"','`',' '] ++
                     ['+','-','_','<','>','{','}','[',']','.',',','~',':','=']

okChar :: Char -> Bool
okChar x = x `elem` fieldCharWhitelist

editUpdate :: Char -> (Event, Transition TrackImportState b)       
editUpdate ch = (chEvent ch , Update (apply (insertChar ch)))

insertChar :: Char -> TrackImportState -> TrackImportState
insertChar ch tis = TIS { trackInfo = trackInfo tis
                        , fields = applyToFocus
                                     (smartInsert ch)
                                     (fields tis) }
    where
      smartInsert :: Char -> CursorString -> CursorString
      smartInsert ch pl = focusRight (insertLeft ch pl)
                         
---------------    
-- Rendering --
---------------

renderTrackImportState :: TrackImportState -> Picture
renderTrackImportState tis = picForImage $ 
                             img (location (trackInfo tis)) <->
                             renderFields (fields tis)

renderFields :: PointedList CursorString -> Image
renderFields pl = box $ vertCat $ toList $ 
                    replace (boldBoxed (_focus pl)) (fmap toBoxed pl)
    where
      toBoxed :: CursorString -> Image
      toBoxed s = box $ doThings s
      boldBoxed :: CursorString -> Image
      boldBoxed s = boldBox $ doThings $ indicateCursor s
      doThings :: CursorString -> Image
      doThings s = resize fieldWidth 1 $ img $ toList s

-- The cursor doesn't act quite right...
-- it has one possible position for each character in the string,
-- but we need it to have n+1...   
indicateCursor :: CursorString -> CursorString
indicateCursor pl = applyToFocus (\x -> '_') pl               
    
toList :: PointedList a -> [a]
toList pl = reverse (_reversedPrefix pl) ++ [(_focus pl)] ++ (_suffix pl)
    
fieldWidth :: Int
fieldWidth = 20           
   

-------------------------------------
-- State and State Transformations --
-------------------------------------

-- a change is needed: Fields always need to have an
-- imaginary blank character at the end to accomodate the
-- cursor (which means for editing too!). Thus, fields can
-- never be empty.   
        
applyToFocus :: (a -> a) -> PointedList a -> PointedList a
applyToFocus f pl = replace (f (_focus pl)) pl

-- always contains a space for the cursor!             
type CursorString = PointedList Char

toCursorString :: String -> CursorString
toCursorString s = unsafeFromList $ s ++ [' ']

fromCursorString :: CursorString -> String
fromCursorString = init . toList                  
     
cursorRight :: TrackImportState -> TrackImportState
cursorRight tis = TIS { trackInfo = trackInfo tis
                      , fields = applyToFocus focusRight (fields tis) }
            
cursorLeft :: TrackImportState -> TrackImportState
cursorLeft tis = TIS { trackInfo = trackInfo tis
                     , fields = applyToFocus focusLeft (fields tis) }
           
focusRight :: CursorString -> CursorString
focusRight pl = case next pl of
                  Nothing -> pl
                  Just nl -> nl

focusLeft :: CursorString -> CursorString
focusLeft pl = case previous pl of
                 Nothing -> pl
                 Just nl -> nl
     
-- the string in focus has a cursor, so CursorString        
generateFields :: Track -> PointedList CursorString
generateFields t = unsafeFromList $ map toCursorString
                     [ title t
                     , concat (intersperse " & " (artists t))
                     , genre t ]               

unsafeFromList :: [a] -> PointedList a
unsafeFromList = (\(Just x) -> x) . fromList

focusUp :: TrackImportState -> TrackImportState
focusUp tis = TIS { trackInfo = trackInfo tis
                  , fields = C.previous (fields tis) }

focusDown :: TrackImportState -> TrackImportState
focusDown tis = TIS { trackInfo = trackInfo tis
                    , fields = C.next (fields tis) }

data TrackImportState = TIS { trackInfo :: Track
                            , fields :: PointedList CursorString }
        
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
      
             
                      
