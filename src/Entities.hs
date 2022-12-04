{-# LANGUAGE TemplateHaskell #-}

module Entities (Game, refresh, groundHeight, cactusPos, dinoPos, initGame, dinoJump) where

import Lens.Micro ((%~), (&), (.~), (^.))
import Lens.Micro.TH (makeLenses)
import Linear.V2 (V2 (..))
import System.Random

data Movement = Jumping | Falling | Ducking | Normal deriving (Eq, Show)

type Pos = V2 Int

data Game = Game
  { -- |  position of cactus (maybe a list later)
    _cactusPos :: [Pos],
    -- | position of dino
    _dinoPos :: Pos,
    -- | movement of dino
    _dinoMvmt :: Movement,
    -- | psudo random number generator
    _randGen :: StdGen
  }
  deriving (Show)

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

dinoJumpPeakHeight :: Int
dinoJumpPeakHeight = 0

initGame :: IO Game
initGame = do
  let g =
        Game
          { _cactusPos = [V2 groundLength groundHeight],
            _dinoPos = defaultDinoPos,
            _dinoMvmt = Normal,
            _randGen = mkStdGen 12345
          }
  return g

-- Refresh game states on each tick
refresh :: Game -> Game
refresh = refreshCactus . refreshDino

refreshCactus :: Game -> Game
refreshCactus = moveCactus . deleteCactus . genCactus

moveCactus :: Game -> Game
moveCactus g = g & cactusPos %~ map f
  where
    f (V2 x y) = V2 (x -1) y

deleteCactus :: Game -> Game
deleteCactus g = g & cactusPos %~ f
  where
    f [] = []
    f posList@((V2 x _):rest)
      | x<0 = rest
      | otherwise = posList

genCactus :: Game -> Game
genCactus g
  | null posList || (getV2x (last posList) < (groundLength - minObstacleDistance)) = g & cactusPos .~ newPos & randGen .~ newGen
  | otherwise = g
    where
      posList = g ^. cactusPos
      (newX, newGen) = randomR (0, maxObstacleDistance - minObstacleDistance) (g ^. randGen)
      newPos = (g ^. cactusPos) ++ [V2 (groundLength + newX) groundHeight]



refreshDino :: Game -> Game
refreshDino g = case g ^. dinoMvmt of
  Jumping ->
    if getDinoHeight g == dinoJumpPeakHeight
      then setDinoMvt g Falling
      else moveDino g (-1)
  Falling ->
    if getDinoHeight g == groundHeight
      then setDinoMvt g Normal
      else moveDino g 1
  _other -> g

moveDino :: Game -> Int -> Game
moveDino g delta = g & dinoPos .~ V2 (getV2x defaultDinoPos) (getDinoHeight g + delta)

getDinoHeight :: Game -> Int
getDinoHeight g = getV2y (g ^. dinoPos)

getV2x :: V2 Int -> Int
getV2x (V2 x _) = x

getV2y :: V2 Int -> Int
getV2y (V2 _ y) = y

setDinoMvt :: Game -> Movement -> Game
setDinoMvt g mvt = g & dinoMvmt .~ mvt

dinoJump :: Game -> Game
dinoJump g = case g ^. dinoMvmt of
  Normal -> setDinoMvt g Jumping
  Ducking -> setDinoMvt g Jumping
  _other -> g
