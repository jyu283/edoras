module UI (main) where

import Brick
import Brick.BChan (newBChan, writeBChan)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void)
import Emoticon (cactus1Widget, cactus2Widget, dino1Widget, bird1Widget, ground1Widget,dino1DuckWidget,gameStartWidget,gameOverWidget,normalBoardWidget)
import Entities
import qualified Graphics.Vty as V
import Graphics.Vty.Attributes (defAttr)
import Lens.Micro ((^.))
import Linear.V2 (V2 (..))

data Tick = Tick

type Name = String -- attribute name type

main :: IO ()
main = do
  chan <- newBChan 10
  _ <- forkIO $
    forever $ do
      writeBChan chan Tick
      threadDelay 20000 -- decides how fast your game moves
  g <- initGame
  let builder = V.mkVty V.defaultConfig
  initialVty <- builder
  void $ customMain initialVty builder (Just chan) app g

app :: App Game Tick Name
app =
  App
    { appDraw = drawUI,
      appChooseCursor = neverShowCursor,
      appHandleEvent = handleEvent,
      appStartEvent = return,
      appAttrMap = const (attrMap defAttr [])
    }

drawUI :: Game -> [Widget Name]
drawUI g =
  [placeWidget (g ^. dinoPos) (g ^. dinoWidget)]  ++
  [placeWidget (V2 50 4) (g ^. boardWidget)] ++
  map (uncurry placeWidget) (g ^. obstacleList) ++
  [placeWidget (V2 0 (groundHeight + 8)) ground1Widget]


placeWidget :: V2 Int -> Widget Name -> Widget Name
placeWidget (V2 x y) = translateBy (Location (x, y))

handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g (AppEvent Tick) = continue $ refresh g
handleEvent g (VtyEvent (V.EvKey V.KUp [])) = continue $ dinoJump g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleEvent g (VtyEvent (V.EvKey V.KDown [])) = continue $ dinoDuck g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'z') [])) = continue $ dinoNormal g
handleEvent g (VtyEvent (V.EvKey (V.KEnter) [])) = continue $ gameStart g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'x') [])) = continue $ gameReady g
handleEvent g _ = continue g

