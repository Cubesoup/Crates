{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}    
        
import Graphics.Vty
import Screen
import Rendering       
import Import
import System.Directory (getCurrentDirectory)       

main = do
     config <- standardIOConfig
     vty <- mkVty config
     runScreen splashScreen vty ()
     shutdown vty
     
-----------------------       
-- The Splash Screen --
-----------------------   
       
splashScreen :: Screen ()
splashScreen = Screen { renderer = (\_ -> picForImage splashImage)
                      , eventMap = splashEventMap }

splashEventMap vty = toMap [ (ctrl (chEvent 'q') , Terminate)
{-                           , (ctrl (chEvent 'm') , Transition { before = (\_ -> importSetup)
                                                              , after  = (\a b -> return ())
                                                              , subScreen = directoryBrowser }) -}
                           , (ctrl (chEvent 'i') , displayHelp importHelp)
                           , (ctrl (chEvent 's') , Transition { before = (\_ -> return "")
                                                              , after  = (\_ _ -> return ())
                                                              , subScreen = searchScreen }) 
                           
                           , ((chEvent '?')      , displayHelp splashHelp)
                           ]

               
splashImage :: Image 
splashImage = img "Welcome to" <->
              pad 10 0 0 0 (vlist    -- it looks better when printed
                 [ "_________                __                 " ,
                   "\\_   ___ \\____________ _/  |_  ____   ______" ,
                   "/    \\  \\/\\_  __ \\__  \\\\   __\\/ __ \\ /  ___/" ,
                   "\\     \\____|  | \\// __ \\|  | \\  ___/ \\___ \\ " ,          
                   " \\______  /|__|  (____  /__|  \\___  >____  >" ,
                   "        \\/            \\/          \\/     \\/ " ])

splashHelp :: Image
splashHelp = vlist [ "Splash screen help page. Type ? to return to the splash screen"
                   , ""
                   , "  C-q   - Quit Crates"
                   , "  C-s   - Open the search screen" 
                   , "  ?     - Show this help page" ]


-----------------------                
-- The Search Screen --
-----------------------                

searchScreen :: Screen String
searchScreen = Screen { renderer = (\text -> picForImage (box (resize 30 1 (img (text ++ "█"))))) -- search box size is a concern, should it
                      , eventMap = searchEventMap }                                      -- bound the length of the search string? NO
                                                                                         -- need a CURSOR -- Vty.Output has cursor things.

searchEventMap vty = toMap $ [ (ctrl (chEvent 'q') , Terminate)
                             , (chEvent '?'        , displayHelp searchHelp)
                             , (backspace          , Update (apply safeInit))
                             , (ctrl (chEvent 'k') , Update (\_ -> return ""))
                             , (meta backspace     , Update (apply (unwords . safeInit . words)))
                             ] ++ alphabetEvents

safeInit :: [a] -> [a]
safeInit [] = []
safeInit xs = init xs        
               
alphabetEvents = map toEventPair $ allowedSearchChars
  where
    toEventPair ch = (chEvent ch , Update (\str -> return (str ++ [ch])))

allowedSearchChars = ['A'..'Z'] ++ ['a'..'z'] ++ ['!','-','@',' ']
               
searchHelp :: Image
searchHelp = vlist [ "Search screen help page. Type ? to return to the search screen"
                   , ""
                   , "Search String Manipulation:"
                   , ""
                   , "  Backspace   - delete last character in the search string"
                   , "  M-Backspace - delete the last word in the search string"
                   , "  C-k         - delete the entire search string"
                   , ""
                   , "Result List Manipulation:"
                   , ""
                   , "Other Commands:"
                   , "  ?           - show this help page"
                   , "  C-q         - quit to the splash screen" ]


-----------------------           
-- The Import Screen --
-----------------------

-- for managing files: add, remove, edit entries. Create matching file structure on disk (later).

   
