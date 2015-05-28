module Import where

import Prelude hiding (length)       
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
    toMap $ [ (escape             , Terminate)
            , (tab                , Update (apply focusDown))
            , (backtab            , Update (apply focusUp))
            , (up                 , Update (apply focusUp))
            , (down               , Update (apply focusDown))
            , (kright             , Update (apply cursorRight))
            , (kleft              , Update (apply cursorLeft))
            , (backspace          , Update (apply deleteAtCursor))
            , (ctrl (chEvent 'a') , Update (apply toFieldStart))
            , (ctrl (chEvent 'e') , Update (apply toFieldEnd))
            , (ctrl (chEvent 'k') , Update (apply deleteToRight))
            , (ctrl (chEvent 'f') , Update (apply cursorRight))
            , (ctrl (chEvent 'b') , Update (apply cursorLeft))
            , (ctrl (chEvent 'p') , Update (apply focusUp))
            , (ctrl (chEvent 'n') , Update (apply focusDown))
            ] ++ charInsertEvents

                                      
    
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
      smartInsert :: Char -> ScrollBox -> ScrollBox
      smartInsert ch sb = focusRight $ sb { boxText = (insertLeft ch (boxText sb)) }
                             
                         
---------------    
-- Rendering --
---------------

renderTrackImportState :: TrackImportState -> Picture
renderTrackImportState tis = picForImage $ 
                             img (location (trackInfo tis)) <->
                             renderFields (fields tis)

renderFields :: PointedList ScrollBox -> Image
renderFields pl = box $ vertCat $ toList $ 
                    replace (boldBoxed (_focus pl)) (fmap toBoxed pl)
    where
      toBoxed :: ScrollBox -> Image
      toBoxed s = box $ doThings s
      boldBoxed :: ScrollBox -> Image
      boldBoxed s = boldBox $ doThings $ s { boxText = indicateCursor (boxText s)}
      doThings :: ScrollBox -> Image
      doThings s = resize fieldBoxWidth 1 $ img $
                     drop (scrollCounter s) (toList (boxText s))

indicateCursor :: CursorString -> CursorString
indicateCursor pl = applyToFocus (\x -> '_') pl               
    
toList :: PointedList a -> [a]
toList pl = reverse (_reversedPrefix pl) ++ [(_focus pl)] ++ (_suffix pl)
    
   
-------------------------------------
-- State and State Transformations --
-------------------------------------

fieldBoxWidth :: Int
fieldBoxWidth = 20           

deleteToRight :: TrackImportState -> TrackImportState
deleteToRight tis = TIS { trackInfo = trackInfo tis
                        , fields = applyToFocus focusDropRight (fields tis) }

focusDropRight :: ScrollBox -> ScrollBox
focusDropRight sb = sb { boxText = dropSuffix (boxText sb) }
              
dropSuffix :: PointedList a -> PointedList a
dropSuffix pl = PointedList (_reversedPrefix pl) (_focus pl) []
          
               
toFieldEnd :: TrackImportState -> TrackImportState    
toFieldEnd tis = TIS { trackInfo = trackInfo tis
                     , fields = applyToFocus toFocusEnd (fields tis) }

toFocusEnd :: ScrollBox -> ScrollBox
toFocusEnd sb = SB { boxText = (\(Just x) -> x) $ moveTo ((length (boxText sb)) - 1)
                                                         (boxText sb)
                   , scrollCounter = if ((length (boxText sb)) - fieldBoxWidth) < 0
                                       then 0
                                       else (length (boxText sb)) - fieldBoxWidth }
           
toFieldStart :: TrackImportState -> TrackImportState
toFieldStart tis = TIS { trackInfo = trackInfo tis
                       , fields = applyToFocus toFocusStart (fields tis) }

toFocusStart :: ScrollBox -> ScrollBox
toFocusStart sb = SB { boxText = (\(Just x) -> x) $ moveTo 0 (boxText sb)
                     , scrollCounter = 0 }
    
deleteAtCursor :: TrackImportState -> TrackImportState
deleteAtCursor tis = TIS { trackInfo = trackInfo tis
                         , fields = applyToFocus
                                      updateBox
                                      (fields tis) }
    where
      deleteFocus :: CursorString -> CursorString
      deleteFocus pl | (atStart pl) && (atEnd pl) = pl
                     | (atStart pl) = (\(Just x) -> x) $ deleteRight pl
                     | otherwise =  (\(Just x) -> x) $ deleteLeft pl
      updateBox :: ScrollBox -> ScrollBox
      updateBox sb = SB { boxText = deleteFocus (boxText sb)
                        , scrollCounter =
                            if index (deleteFocus (boxText sb)) < (scrollCounter sb)
                              then (scrollCounter sb) - 1
                              else (scrollCounter sb) }

   
applyToFocus :: (a -> a) -> PointedList a -> PointedList a
applyToFocus f pl = replace (f (_focus pl)) pl

-- always contains a space for the cursor!             
type CursorString = PointedList Char

toCursorString :: String -> CursorString
toCursorString s = unsafeFromList $ s ++ [' ']

fromCursorString :: CursorString  -> String
fromCursorString = init . toList                  

toScrollBox :: String -> ScrollBox
toScrollBox s = SB { boxText = toCursorString s
                   , scrollCounter = 0 }

fromScrollBox :: ScrollBox -> String
fromScrollBox b = fromCursorString (boxText b)              


cursorRight :: TrackImportState -> TrackImportState
cursorRight tis = TIS { trackInfo = trackInfo tis
                      , fields = applyToFocus focusRight (fields tis) }
            
cursorLeft :: TrackImportState -> TrackImportState
cursorLeft tis = TIS { trackInfo = trackInfo tis
                     , fields = applyToFocus focusLeft (fields tis) }
           
focusRight :: ScrollBox -> ScrollBox
focusRight sb = case next (boxText sb) of
                  Nothing -> sb
                  Just nl -> SB { boxText = nl
                                , scrollCounter = if ((index nl) - (scrollCounter sb)) >= fieldBoxWidth
                     then (scrollCounter sb) + 1
                     else (scrollCounter sb)
          }

focusLeft :: ScrollBox -> ScrollBox
focusLeft sb = case previous (boxText sb) of
                 Nothing -> sb
                 Just nl -> SB { boxText = nl
                               , scrollCounter = if ((index nl) - (scrollCounter sb)) < 0
                     then (scrollCounter sb) - 1
                     else (scrollCounter sb)
          }
     
-- the string in focus has a cursor, so CursorString        
generateFields :: Track -> PointedList ScrollBox
generateFields t = unsafeFromList $ map toScrollBox
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
                            , fields :: PointedList ScrollBox }

data ScrollBox = SB { boxText :: CursorString
                    , scrollCounter :: Int }

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
      
             
                      
