{-# LANGUAGE TemplateHaskell #-}

module Entities 
  ( Game,
    refresh,
    groundHeight,
    obstacleList,
    dinoPos,
    birdPos,
    dinoWidget,
    boardWidget,
    isOver,
    dinoJump,
    dinoDuck,
    dinoNormal,
    changeBoard,
    gameStart,
    gameReady,
    newGame
  ) 
where

import Brick
import Emoticon
import Lens.Micro ((%~), (&), (.~), (^.))
import Lens.Micro.TH (makeLenses)
import Linear.V2 (V2 (..))
import System.Random

data Movement = Ducking | Jumping | Normal deriving (Eq, Show)

type Pos = V2 Int

data Game = Game
  {
    -- | a list of obstacles and their positions
    _obstacleList :: [(Pos, Widget String)],
    -- | position of dino
    _dinoPos :: Pos,
    -- | velocity of dino
    _dinoVelocity :: Int,
    -- | game ticks
    _tick :: Int,
    -- | position of bird
    _birdPos :: Pos,
    -- | movement of dino
    _dinoMvmt :: Movement,
    -- | psudo random number generator
    _randGen :: StdGen,
    -- | dino widget
    _dinoWidget :: Widget String,
    -- | board widget
    _boardWidget :: Widget String,
    -- | the state of game 0-ready to start 1 - in the process 2-end
    _isOver :: Int,
    -- | bird widget
    _birdWidget :: Widget String
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

newGame :: Game
newGame = 
  Game {
    _obstacleList = [],
    _dinoPos = defaultDinoPos,
    _dinoVelocity = 0,
    _tick = 0,
    _birdPos = V2 250 8,
    _dinoMvmt = Normal,
    _randGen = mkStdGen 12345,
    _dinoWidget = dino1Widget,
    _boardWidget = gameStartWidget,
    _isOver = 0,
    _birdWidget = bird1Widget
  }

-- Refresh game states on each tick
refresh :: Game -> Game
refresh = tickincr . refreshDinoWidget . refreshDino . refreshObstacle . detectCollision

tickincr :: Game -> Game
tickincr g = g & tick %~ incr
  where
    incr x = x + 1

detectCollision :: Game -> Game
detectCollision g = if null (g ^. obstacleList) then g else detectCollision' g

detectCollision' :: Game -> Game
detectCollision' g =
  if noHit (g ^. dinoWidget) (g ^. dinoPos) (getFirstObstacleWidget g) (getFirstObstaclePos g) then
    g
  else
    gameReady g 

noHit :: Widget String -> Pos -> Widget String -> Pos -> Bool
noHit w1 p1@(V2 x1 y1) w2 p2@(V2 x2 y2) = 
  -- x1 + hSize w1 < x2 || x1 > x2 + hSize w1 || y1 + vSize w1 < y2 || y1 > y2 + vSize w2 
  x1 + 24 < x2 || x1 > x2 + 15 || y1 + 5 < y2 || y1 > y2 + 8

getFirstObstaclePos :: Game -> Pos
getFirstObstaclePos g = fst (head (g ^. obstacleList))

getFirstObstacleWidget :: Game -> Widget String
getFirstObstacleWidget g = snd (head (g ^. obstacleList))

refreshDinoWidget :: Game -> Game
refreshDinoWidget g
  | (g ^. dinoMvmt) /= Jumping = setRunningDinoWidget g
  | otherwise = g & dinoWidget .~ dino1Widget
  where
    setRunningDinoWidget g
      | (g ^. dinoMvmt == Normal) && (g ^. tick) `mod` 14 < 7 = g & dinoWidget .~ dino2Widget
      | (g ^. dinoMvmt == Normal) && (g ^. tick) `mod` 14 >= 7 = g & dinoWidget .~ dino3Widget
      | (g ^. dinoMvmt == Ducking) && (g ^. tick) `mod` 14 < 7 = g & dinoWidget .~ dino1DuckWidget
      | (g ^. dinoMvmt == Ducking) && (g ^. tick) `mod` 14 >= 7 = g & dinoWidget .~ dino2DuckWidget
      | otherwise = g & dinoWidget .~ dino1DuckWidget

refreshObstacle :: Game -> Game
refreshObstacle g
 | g ^. isOver == 1  =   moveObstacle (deleteObstacle (genObstacle g))
 |otherwise = g

moveObstacle :: Game -> Game
moveObstacle g = g & obstacleList %~ map f
  where
    f (V2 x y, w) = (V2 (x -1) y, w)

deleteObstacle :: Game -> Game
deleteObstacle g = g & obstacleList %~ f
  where
    f [] = []
    f obList@((V2 x _, _) : rest)
      | x < 0 = rest
      | otherwise = obList

genObstacle :: Game -> Game
genObstacle g
  | null obList || (getV2x (fst $ last obList) < (groundLength - minObstacleDistance))
    = g & obstacleList .~ newObList & randGen .~ newGen
  | otherwise = g
  where
    obList = g ^. obstacleList
    (newX, tmpGen) = randomR (0, maxObstacleDistance - minObstacleDistance) (g ^. randGen)
    (widgetIdx, newGen) = randomR (1, 3 :: Int) tmpGen
    newOb = case widgetIdx of
      1 -> (V2 (groundLength + newX) groundHeight, cactus1Widget)         -- single cactus
      2 -> (V2 (groundLength + newX) (groundHeight-4), cactus2Widget)     -- two cacti
      3 -> (V2 (groundLength + newX) (groundHeight-5), g ^. birdWidget)   -- bird
      _ -> (V2 (groundLength + newX) groundHeight, cactus1Widget)
    newObList = (g ^. obstacleList) ++ [newOb]

refreshDino :: Game -> Game
refreshDino g =
  if (g ^. tick `mod` 4 == 0) && (g ^. dinoMvmt == Jumping)
    then _refreshDino g
    else g

_refreshDino :: Game -> Game
_refreshDino = moveDino . updateDinoVelocity . updateDinoMvnt

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

updateDinoMvnt :: Game -> Game
updateDinoMvnt g
  | (g ^. dinoMvmt == Jumping) && (getDinoHeight g == groundHeight) && (g ^. dinoVelocity > 0) = g & dinoMvmt .~ Normal
  | otherwise = g

getDinoHeight :: Game -> Int
getDinoHeight g = getV2y (g ^. dinoPos)

getV2x :: V2 Int -> Int
getV2x (V2 x _) = x

getV2y :: V2 Int -> Int
getV2y (V2 _ y) = y

dinoJump :: Game -> Game
dinoJump g
  | g ^. isOver == 1 = if getDinoHeight g < groundHeight
    then g
    else g & dinoVelocity .~ dinoJumpInitialVelocity & dinoMvmt .~ Jumping
  |otherwise = g

setDinoPosDuck :: Game -> Game
setDinoPosDuck g = g & dinoPos .~ V2 20 (groundHeight + 5)

setDinoPosNormal :: Game -> Game
setDinoPosNormal g = g & dinoPos .~ V2 20 groundHeight

dinoDuck :: Game -> Game
dinoDuck g
 | g ^. isOver == 1 = setDinoPosDuck (g & dinoMvmt .~ Ducking)
 | otherwise = g

dinoNormal :: Game -> Game
dinoNormal g = setDinoPosNormal (g & dinoMvmt .~ Normal)

changeBoard :: Game -> Game
changeBoard g 
 | g ^. isOver == 0 =  g & boardWidget .~ normalBoardWidget
 | g ^. isOver == 1 = g & boardWidget .~ gameOverWidget
 | g ^. isOver == 2 = g & boardWidget .~ gameStartWidget
 | otherwise = g

changeStateToStart :: Game -> Game
changeStateToStart g = g & isOver .~ 1

changeStateToReady :: Game -> Game
changeStateToReady g = g & isOver .~ 0

gameStart :: Game -> Game
gameStart g
 | g ^. isOver == 0 = changeStateToStart  (changeBoard newGame)
 | otherwise = g

gameReady :: Game -> Game
gameReady = changeStateToReady . changeBoard
