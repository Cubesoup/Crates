module Import where

-- TODO: permissions filtering of directory listing result list.
-- TODO: also filter directory result list so that only music
--       files and directories that contain music files are displayed. (maybe)   

-- TODO: prefix matching on result list (with search string) (should the
--       search string be absolute and respond to directory changes?)

-- TODO: Scrolling search string, scrolling results box.         
         
import Screen
import MusicTypes
import Rendering
import Graphics.Vty       
import System.Directory

import Data.List.Zipper
import Data.List.Split (wordsBy)
import Data.List (intersperse)
       
----------------------------------       
-- The Track Importation Screen --       
----------------------------------

importScreen :: Screen Crate
importScreen = Screen { renderer = importRenderer
                      , eventMap = importEventMap }

importRenderer :: Crate -> Picture             
importRenderer = undefined

importEventMap vty = undefined               

-- we want to be able to add directories. Thus,
-- a directory browser is required.

  
importSetup :: IO DirState
importSetup = do curr <- getCurrentDirectory
                 results <- getDirectoryContents curr
                 return (curr,fromList results)

directoryBrowser :: Screen DirState
directoryBrowser = Screen { renderer = renderDirectories
                          , eventMap = directoryEventMap }

renderDirectories :: DirState -> Picture               
renderDirectories (q,a) = picForImage ((box (img q)) <-> (renderResults a))

renderResults :: Zipper String -> Image
renderResults z = vlist $ toList $ replace (">>" ++ safeStr) z
  where
    safeStr :: String
    safeStr = case safeCursor z of
              Nothing -> "No Results"
              Just s  -> s
              
directoryEventMap vty = toMap $ [ (ctrl (chEvent 'q') , Terminate)
                                , (chEvent '?'        , displayHelp importHelp)
                                , (ctrl (chEvent 'n') , cursorDown)
                                , (ctrl (chEvent 'p') , cursorUp)
                                , (up                 , cursorUp)
                                , (down               , cursorDown)
                                , (enter              , toSubDir)
                                , (kright             , toSubDir)
                                , (backspace          , toParentDir)
                                , (kleft              , toParentDir)
                                ] ++ dirCharEvents


toParentDir = Update $ (\(a,z) -> 
                if a == "/" then return (a,z)  -- root has no parent! should probably also deal with permissions
                else do
                     let a' = ['/'] ++ (concat $ intersperse ['/'] $ init (wordsBy (\x -> x == '/') a))
                     setCurrentDirectory a'
                     results <- getDirectoryContents a'
                     return (a' , fromList results))
            
-- a consequence of setting the application directory is that
-- the import screen rememebers where you were last. If this is
-- undesirable, you have to change the directory after exiting
-- the screen. Could also change before entering the screen to
-- use a user configurable directory instead of application directory.   
toSubDir = Update $ (\(a,z) -> 
               do
                 isDir <- doesDirectoryExist (cursor z)
                 if isDir
                    then do
                         setCurrentDirectory (cursor z)
                         a' <- getCurrentDirectory
                         results <- getDirectoryContents a'
                         return (a', fromList results)
                    else return (a,z) -- maybe print something somewhere??
                       )
                       
cursorUp = Update (apply (\(a,b) -> (a, left b)))                  

cursorDown = Update (apply (\(a,b) -> (a, rightButOne b)))                  
  where rightButOne :: Zipper a -> Zipper a
        rightButOne (Zip [] []) = Zip [] []            
        rightButOne (Zip xs [x]) = Zip xs [x]
        rightButOne xs = right xs            

           
type DirState = (String,Zipper String)

-- like right but stops going right one before the end     
     
dirCharEvents = map toEventPair $ allowedDirChars
  where
    toEventPair ch = (chEvent ch , Update (\(str,a) -> return ((str ++ [ch]),a)))

allowedDirChars = ['A'..'Z'] ++ ['a'..'z'] ++ ['!','-','@',' ']
     
importHelp :: Image
importHelp = vlist [ "Import screen help page. Type ? to return to the import screen"
                   , ""
                   , "Search Directory Manipulation:"
                   , ""
                   , "Result List Manipulation:"
                   , ""
                   , "  C-n / ↑        - focus next result"
                   , "  C-p / ↓        - focus previous result"
                   , "  Enter / →      - look at focused directory"
                   , "  Backspace / ←  - look at parent directory" 
                   , ""
                   , "Other Commands:"
                   , "  ?    - show this help page"
                   , "  C-q  - quit to the splash screen" ] 



     
                  
