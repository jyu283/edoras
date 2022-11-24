module UI (main) where

import Dino
import Emoticon ( dino1Widget, cactus1Widget, ground1Widget )

import Brick
import Brick.BChan (newBChan, writeBChan)
import qualified Graphics.Vty as V
import Graphics.Vty.Attributes ( defAttr )
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (forever, void)
import Lens.Micro ((^.))
import Linear.V2 (V2(..))

data Tick = Tick
type Name = String -- attribute name type

main :: IO ()
main = do
  chan <- newBChan 10
  forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 20000 -- decides how fast your game moves
  g <- initGame
  let builder = V.mkVty V.defaultConfig
  initialVty <- builder
  void $ customMain initialVty builder (Just chan) app g

app :: App Game Tick Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const (attrMap defAttr [])
          }

drawUI :: Game -> [Widget Name]
drawUI g = [placeWidget (g ^. dinoPos) dino1Widget, placeWidget (g ^. cactusPos) cactus1Widget, placeWidget (V2 0 13) ground1Widget]

placeWidget :: V2 Int -> Widget Name -> Widget Name
placeWidget (V2 x y) = translateBy (Location (x, y))

handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g (AppEvent Tick)                       = continue $ step g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleEvent g _ = continue g
