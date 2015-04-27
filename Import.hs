module Import where
     
import Screen
import MusicTypes
import Rendering
import Graphics.Vty       
import System.Directory
       
----------------------------------       
-- The Track Importation Screen --       
----------------------------------

importScreen :: Screen Crate
importScreen = Screen { renderer = importRenderer
                      , eventMap = importEventMap }

importRenderer :: Crate -> Picture             
importRenderer = undefined

importEventMap vty = undefined               

               
