module BrowseDir where
       
-- Possibility: Parameterize directoryBrowser by a filter. This one filters music, but with minimal changes
-- could be made to filter anything. (a filter has type FilePath -> Bool).   
       
-- TODO: prefix matching on result list (with search string) (should the
--       search string be absolute and respond to directory changes?)

import MusicTypes (emptyCrate)         
import Screen
import Rendering
import Graphics.Vty       
import System.Directory
import System.FilePath
import Control.Monad (filterM)
import qualified Data.List.PointedList as PL
import Data.List (sort)
import Data.Char (toLower)
import qualified Data.Set as Set
import Import       
       
-- The idea here is that a user probably has access to their
-- home directory, so this shouldn't explode.
dirBrowseSetup :: IO DirectoryBrowserState
dirBrowseSetup = do
    home <- getHomeDirectory
    setCurrentDirectory home
    folderDisp <- contents home
    return $ DS { folder = folderDisp
                , path = home }
    

directoryBrowser :: Screen DirectoryBrowserState
directoryBrowser = Screen { renderer = renderBrowserState
                          , eventMap = directoryEventMap }


focusApply :: (a -> a) -> PL.PointedList a -> PL.PointedList a
focusApply f pl = PL.replace (f (PL._focus pl)) pl           

directoryEventMap vty = toMap $ [ (escape             , Terminate)
                                , (chEvent '?'        , displayHelp importHelp)
                                , (ctrl (chEvent 'n') , Update (apply cursorDown))
                                , (ctrl (chEvent 'p') , Update (apply cursorUp))
                                , (up                 , Update (apply cursorUp))
                                , (down               , Update (apply cursorDown))
                                , (tab                , Update followCursor)
                                , (kright             , Update followCursor)
                                , (enter              , Update followCursor)
                                , (meta backspace     , Update toParentDir)
                                , (kleft              , Update toParentDir)
                                , (backtab            , Update toParentDir)
                                , (ctrl (chEvent 'g') , Transition { before = toImportState
                                                                   , after = (\x _ -> return x)
                                                                   , subScreen = importTrack emptyCrate })
                                {- , (ctrl (chEvent 'a') , importCursor)
                                , (chEvent '/'        , dirSepChar)
                                , (backspace          , delSearchChar) -}
                                ] -- ++ dirCharEvents

toImportState :: DirectoryBrowserState -> IO TrackImportState
toImportState dbs = setupTrackImport $ (path dbs) </> takeFileName (PL._focus (folderContents (folder dbs)))
                  
toParentDir :: DirectoryBrowserState -> IO DirectoryBrowserState
toParentDir dbs = do
            let newPath = takeDirectory (path dbs)
            newFolder <- contents newPath
            if newFolder == Empty
              then return dbs -- don't go to unuseable parent folders!
              else return $ DS { folder = newFolder
                               , path = newPath }

followCursor :: DirectoryBrowserState -> IO DirectoryBrowserState
followCursor dbs = do
          if (folder dbs) == Empty then return dbs
            else do
             let newPath = (path dbs) </> takeFileName (PL._focus (folderContents (folder dbs)))
             notADirectory <- doesFileExist newPath
             if notADirectory then return dbs
               else do
                 newFolder <- contents newPath
                 return $ DS { folder = newFolder
                             , path = newPath }

cursorDown :: DirectoryBrowserState -> DirectoryBrowserState
cursorDown dbs = dbs { folder = moveFocusDown (folder dbs) }

cursorUp :: DirectoryBrowserState -> DirectoryBrowserState
cursorUp dbs = dbs { folder = moveFocusUp (folder dbs) }
                     
             
-----------------------------     
-- Directory Browser State --
-----------------------------

renderBrowserState :: DirectoryBrowserState -> Picture
renderBrowserState dbs = picForImage $
                           img (path dbs) <->
                           renderFolder (folder dbs)

renderFolder :: FolderDisplay -> Image
renderFolder Empty = box $ resize (windowWidth) (windowHeight) $ img "No Music Here!"
renderFolder fd = box $ resize (windowWidth) (windowHeight) $ vlist flatList
  where
    flatList = take windowHeight $ drop (displayCounter fd) $ (toList (focusApply (\x -> ">" ++ x) (fmap takeFileName (folderContents fd))))

        
data DirectoryBrowserState = DS { folder :: FolderDisplay
                                , path :: String }
         
-- only call on directories that you know exist, and know are readSearchable.
contents :: FilePath -> IO FolderDisplay
contents path = do
         localThings <- getDirectoryContents path
         let things = sort $ map (\x -> path </> x) localThings
         okThings <- filterM isOKThing things
         case okThings of
             [] -> return Empty
             xs -> return $ FD { folderContents = (\(Just x) -> x) $ PL.fromList okThings
                               , displayCounter = 0 }         

-- a pointedList is a lot like a zipper. the displayCounter allows
-- more sophisticated scrolling behaviour. the displayCounter should
-- always contain the index of the first element in the display
-- window.  
data FolderDisplay = Empty 
                   | FD { folderContents :: PL.PointedList String
                        , displayCounter :: Int -- the index of the first item in the display window.
                        }
     deriving (Eq, Show)

topBuffer :: Int
topBuffer = 2

bottomBuffer :: Int
bottomBuffer = 3

windowHeight :: Int
windowHeight = 10

windowWidth :: Int
windowWidth = 20            

moveFocusDown :: FolderDisplay -> FolderDisplay
moveFocusDown Empty = Empty
moveFocusDown fd = case (PL.next (folderContents fd)) of
                     Nothing -> fd                   -- if at end of list do nothing
                     Just pl -> FD { folderContents = pl
                                   , displayCounter = if (displayCounter fd) + windowHeight - bottomBuffer < (PL.index pl)
                                                         then (displayCounter fd) + 1
                                                         else displayCounter fd
                                   }
              
moveFocusUp :: FolderDisplay -> FolderDisplay              
moveFocusUp Empty = Empty
moveFocusUp fd = case (PL.previous (folderContents fd)) of
                   Nothing -> fd
                   Just pl -> FD { folderContents = pl
                                 , displayCounter = if (PL.index pl) < ((displayCounter fd) + topBuffer)
                                                        then (displayCounter fd) - 1
                                                        else displayCounter fd
                                 }

------------------------
-- Detect Music Files --
------------------------

-- a directory contains music if it is an OK directory and at least one thing in it is music.
-- This won't really work unless it's recursive. Probably best not to do it yet.   
containsMusic :: FilePath -> IO Bool
containsMusic path = do
    ok <- isOKDirectory path
    case ok of
      False -> return False
      True  -> do
          things <- getDirectoryContents path
          things' <- filterM isOKThing (filter isMusic things)
          case things' of
            [] -> return False
            _  -> return True
    
-- how crazy do we want to be here? video formats? probably just what alsaplayer does.
musicExtensions :: Set.Set String
musicExtensions = Set.fromList 
                [ ".flac"
                , ".mp3"
                , ".ogg"
                , ".oga"
                , ".wav"
                ]

isMusic :: FilePath -> Bool
isMusic path = (takeExtension path) `Set.member` musicExtensions

----------------------------         
-- File Permission Things --
----------------------------            
         
isOKThing :: FilePath -> IO Bool
isOKThing path = do
                 dir <- isOKDirectory path
                 file <- isOKFile path
                 let musicFile = file && (isMusic path) 
                 return $ case takeFileName path of
                   "."   -> False
                   ".."  -> False
                   other -> if (head other) == '.'
                              then False
                              else (dir || musicFile)
                       
isReadSearchable :: FilePath -> IO Bool
isReadSearchable path = do
                 perms <- getPermissions path
                 return (readable perms && searchable perms)

isReadable :: FilePath -> IO Bool
isReadable path = do
           perms <- getPermissions path
           return (readable perms)
                 
isOKFile :: FilePath -> IO Bool
isOKFile path = do
            exists <- doesFileExist path
            case exists of
              False -> return False
              True  -> isReadable path
         
isOKDirectory :: FilePath -> IO Bool
isOKDirectory path = do
              exists <- doesDirectoryExist path
              case exists of
                False -> return False
                True  -> isReadSearchable path
                       

{-matchStr :: (String,Zipper String) -> (String,Zipper String)
matchStr (a,z) = (a , fromList $ filter (\x -> isPrefixOf (map toLower local) (map toLower x)) $ toList z)
  where
    local = if (last a) == '/' then "" else last (wordsBy (\x -> x == '/') a)
         
dirCharEvents = map toEventPair $ allowedDirChars
  where
    toEventPair ch = (chEvent ch , Update (\(str,z) -> return $ matchStr (str ++ [ch],z)))

allowedDirChars = ['A'..'Z'] ++ ['a'..'z'] ++ ['!','-','@',' '] -- plus more! -}

----------------------------                
-- Contextual Help Screen --
----------------------------                

importHelp :: Image
importHelp = vlist [ "Import screen help page. Type ? to return to the import screen"
                   , ""
                   , "Search Directory Manipulation:"
                   , ""
                   , "Result List Manipulation:"
                   , ""
                   , "  C-n / ↑                   - focus next result"
                   , "  C-p / ↓                   - focus previous result"
                   , "  tab / → / enter           - look at focused directory"
                   , "  M-backspace / ← / backtab - look at parent directory" 
                   , ""
                   , "Other Commands:"
                   , "  ?    - show this help page"
                   , "  C-q  - quit to the splash screen" ] 



     
                  
