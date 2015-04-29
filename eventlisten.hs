import Graphics.Vty
import Rendering
       
main = do
     config <- standardIOConfig
     vty <- mkVty config
     main' vty

main' vty = do
      ev <- nextEvent vty
      update vty (picForImage (img (show ev)))
      case ev of
        EvKey KEsc [] -> shutdown vty
        x             -> main' vty
