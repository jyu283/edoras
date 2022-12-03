{-# LANGUAGE TemplateHaskell #-}

module Entities (Game, refresh, groundHeight, cactusPos, dinoPos, initGame, dinoJump) where

import Lens.Micro ((%~), (&), (.~), (^.))
import Lens.Micro.TH (makeLenses)
import Linear.V2 (V2 (..))

data Movement = Jumping | Falling | Ducking | Normal deriving (Eq, Show)

type Pos = V2 Int

data Game = Game
  { -- |  position of cactus (maybe a list later)
    _cactusPos :: Pos,
    -- | position of dino
    _dinoPos :: Pos,
    -- | velocity of dino
    _dinoVelocity :: Int,
    -- | game ticks
    _tick :: Int
  }
  deriving (Show)

makeLenses ''Game -- What's this for?

groundHeight :: Int
groundHeight = 27

defaultDinoPos :: Pos
defaultDinoPos = V2 20 groundHeight

-- dinoJumpPeakHeight :: Int
-- dinoJumpPeakHeight = 0

dinoJumpInitialVelocity :: Int
dinoJumpInitialVelocity = -8

gravity :: Int
gravity = 1

initGame :: IO Game
initGame = do
  let g =
        Game
          { _cactusPos = V2 200 groundHeight,
            _dinoPos = defaultDinoPos,
            _dinoVelocity = 0,
            _tick = 0
          }
  return g

-- Refresh game states on each tick
refresh :: Game -> Game
refresh = tickincr . refreshCactus . refreshDino

tickincr :: Game -> Game
tickincr g = g & tick %~ incr
  where
    incr x = x + 1

refreshCactus :: Game -> Game
refreshCactus g = g & cactusPos %~ f
  where
    f (V2 x y) = V2 ((x -1) `mod` 200) y

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
dinoJump g = g & dinoVelocity .~ dinoJumpInitialVelocity