module DinoApp (app) where

import Brick
import Brick.Widgets.Border (hBorder, vBorder)
import Emoticon
import Graphics.Vty.Attributes
import Linear.V2 (V2 (..))

-- The application state type s:
--    the type of data that will evolve over the course of the application's execution.
--    Your application will provide the library with its starting value and event handling
--    will transform it as the program executes. When a brick application exits, the final
--    application state will be returned.
-- The event type e:
--    the type of custom application events that your application will need to produce and
--    handle in appHandleEvent. All applications will be provided with events from the
--    underlying vty library, such as keyboard events or resize events; this type variable
--    indicates the type of additional events the application will need. For more details,
--    see Using Your Own Event Type.
-- The resource name type n:
--    during application execution we sometimes need a way to refer to rendering state,
--    such as the space taken up by a given widget, the state for a scrollable viewport,
--    a mouse click, or a cursor position. For these situations we need a unique handle
--    called a resource name. The type n specifies the name type the application will use
--    to identify these bits of state produced and managed by the renderer. The resource
--    name type must be provided by your application; for more details, see Resource Names.

type Score = Int

type Position = V2 Int

type Dino = [Position] -- TODO: Maybe not a list?

type Name = ()

data Game = Game
  { _dino :: Dino,
    _score :: Score
  }

initGame :: IO Game
initGame = do
  let g =
        Game
          { _dino = [V2 5 0],
            _score = 0
          }
  return g

data Tick = Tick

app :: App Game Tick Name
app =
  App
    { appDraw = drawUI,
      appChooseCursor = neverShowCursor,
      appHandleEvent = handleEvent,
      appStartEvent = return,
      appAttrMap = const (attrMap defAttr [])
    }

-- dinoMap :: AttrMap
-- dinoMap = undefined

-- TODO
drawUI :: Game -> [Widget Name]
-- drawUI _ = [translateBy (Location (20, 13)) (str Lib.strSmallDino), testWidget, str "Hello," <=> str "World!" <=> hBorder]
drawUI _ = [translateBy (Location (0, 5)) dino1Widget, translateBy (Location (50, 1 + 5)) cactus1Widget, translateBy (Location (0, 8 + 5)) testWidget2]

handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent = undefined

testWidget2 = vBox [row1, row2, row3, row4, row5]
  where
    row1 = str "-----------------------------------------------------------------------------------"
    row2 = str "-----------------------------------------------------------------------------------"
    row3 = str "-----------------------------------------------------------------------------------"
    row4 = str "-----------------------------------------------------------------------------------"
    row5 = str "-----------------------------------------------------------------------------------"
