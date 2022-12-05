{-# LANGUAGE TemplateHaskell #-}

module Entities (Game, refresh, groundHeight, cactusPos, dinoPos, birdPos, dinoWidget,boardWidget,isOver,initGame, dinoJump, dinoDuck, dinoNormal,changeBoard,gameStart,gameReady) where

import Brick
import Emoticon (cactus1Widget, dino1DuckWidget, dino1Widget, ground1Widget,gameStartWidget,gameOverWidget,normalBoardWidget)
import Lens.Micro ((%~), (&), (.~), (^.))
import Lens.Micro.TH (makeLenses)
import Linear.V2 (V2 (..))
import System.Random

data Movement = Ducking | Normal deriving (Eq, Show)

type Pos = V2 Int

data Game = Game
  { -- |  position of cactus (maybe a list later)
    _cactusPos :: [Pos],
    -- | position of dino
    _dinoPos :: Pos,
    -- | velocity of dino
    _dinoVelocity :: Int,
    -- | game ticks
    _tick :: Int,
    -- | position of bird
    _birdPos :: Pos,
    -- | position of board
    _boardPos :: Pos,
    -- | movement of dino
    _dinoMvmt :: Movement,
    -- | psudo random number generator
    _randGen :: StdGen,
    -- | dino widget
    _dinoWidget :: Widget String,
    -- | board widget
    _boardWidget :: Widget String,
    -- | the state of game 0-ready to start 1 - in the process 2-end
    _isOver :: Int
  }

--deriving (Show)

makeLenses ''Game -- What's this for?

groundHeight :: Int
groundHeight = 27

groundLength :: Int
groundLength = 200

minObstacleDistance :: Int
minObstacleDistance = 50

maxObstacleDistance :: Int
maxObstacleDistance = 150

defaultDinoPos :: Pos
defaultDinoPos = V2 20 groundHeight

dinoJumpInitialVelocity :: Int
dinoJumpInitialVelocity = -8



gravity :: Int
gravity = 1

initGame :: IO Game
initGame = do
  let g =
        Game
          { _cactusPos = [V2 groundLength groundHeight],
            _dinoPos = defaultDinoPos,
            _dinoVelocity = 0,
            _tick = 0,
            _birdPos = V2 250 8,
            _dinoMvmt = Normal,
            _randGen = mkStdGen 12345,
            _dinoWidget = dino1Widget,
            _boardWidget = gameStartWidget,
            _isOver = 0
          }
  return g

-- Refresh game states on each tick
refresh :: Game -> Game
refresh = tickincr . refreshCactus . refreshDino . refreshBird

tickincr :: Game -> Game
tickincr g = g & tick %~ incr
  where
    incr x = x + 1

refreshCactus :: Game -> Game
refreshCactus g
  |g ^. isOver == 1  = moveCactus (deleteCactus (genCactus g))
  |otherwise = g

moveCactus :: Game -> Game
moveCactus g = g & cactusPos %~ map f
  where
    f (V2 x y) = V2 (x -1) y

deleteCactus :: Game -> Game
deleteCactus g = g & cactusPos %~ f
  where
    f [] = []
    f posList@((V2 x _) : rest)
      | x < 0 = rest
      | otherwise = posList

genCactus :: Game -> Game
genCactus g
  | null posList || (getV2x (last posList) < (groundLength - minObstacleDistance)) = g & cactusPos .~ newPos & randGen .~ newGen
  | otherwise = g
  where
    posList = g ^. cactusPos
    (newX, newGen) = randomR (0, maxObstacleDistance - minObstacleDistance) (g ^. randGen)
    newPos = (g ^. cactusPos) ++ [V2 (groundLength + newX) groundHeight]

refreshBird :: Game -> Game
refreshBird g
  | g ^. isOver == 1 = g & birdPos %~ f
  | otherwise = g
  where
    f (V2 x y) = V2 ((x -1) `mod` 300) y
 
--refreshBird g = g & birdPos %~ f
--  where
--   f (V2 x y) = V2 ((x -1) `mod` 300) y

refreshDino :: Game -> Game
refreshDino g =
  if g ^. tick `mod` 3 == 0
    then _refreshDino g
    else g

_refreshDino :: Game -> Game
_refreshDino = moveDino . updateDinoVelocity

moveDino :: Game -> Game
moveDino g = g & dinoPos .~ V2 (getV2x defaultDinoPos) new_height
  where
    new_height = min (getDinoHeight g + (g ^. dinoVelocity)) groundHeight

updateDinoVelocity :: Game -> Game
updateDinoVelocity g = g & dinoVelocity .~ new_velocity
  where
    new_velocity =
      if getDinoHeight g <= groundHeight
        then (g ^. dinoVelocity) + gravity
        else 0

getDinoHeight :: Game -> Int
getDinoHeight g = getV2y (g ^. dinoPos)

getV2x :: V2 Int -> Int
getV2x (V2 x _) = x

getV2y :: V2 Int -> Int
getV2y (V2 _ y) = y

dinoJump :: Game -> Game
dinoJump g =
  if getDinoHeight g < groundHeight
    then g
    else g & dinoVelocity .~ dinoJumpInitialVelocity

setDinoWidgetDuck :: Game -> Game
setDinoWidgetDuck g = g & dinoWidget .~ dino1DuckWidget

setDinoWidgetNormal :: Game -> Game
setDinoWidgetNormal g = g & dinoWidget .~ dino1Widget

setDinoPosDuck :: Game -> Game
setDinoPosDuck g = g & dinoPos .~ V2 20 32

setDinoPosNormal :: Game -> Game
setDinoPosNormal g = g & dinoPos .~ V2 20 groundHeight

dinoDuck :: Game -> Game
dinoDuck = setDinoWidgetDuck . setDinoPosDuck

dinoNormal :: Game -> Game
dinoNormal = setDinoWidgetNormal . setDinoPosNormal

changeBoard :: Game -> Game
changeBoard g 
 | g ^. isOver == 0 =  g & boardWidget .~ normalBoardWidget
 | g ^. isOver == 1 = g & boardWidget .~ gameOverWidget
 | g ^. isOver == 2 = g & boardWidget .~ gameStartWidget

changeStateToStart :: Game -> Game
changeStateToStart g = g & isOver .~ 1

changeStateToReady :: Game -> Game
changeStateToReady g = g & isOver .~ 0

gameStart :: Game -> Game
gameStart = changeStateToStart . changeBoard

gameReady :: Game -> Game
gameReady = changeStateToReady . changeBoard
