{-# LANGUAGE TemplateHaskell #-}

module Entities (Game, refresh, cactusPos, dinoPos, initGame, dinoJump) where

import Lens.Micro ((%~), (&))
import Lens.Micro.TH (makeLenses)
import Linear.V2 (V2 (..))

data Movement = Jumping | Falling | Ducking | Normal deriving (Eq, Show)

data Game = Game
  { -- |  position of cactus (maybe a list later)
    _cactusPos :: Pos,
    -- | position of dino
    _dinoPos :: Pos,
    -- | movement of dino
    _dinoMvmt :: Movement
  }
  deriving (Show)

type Pos = V2 Int

makeLenses ''Game

initGame :: IO Game
initGame = do
  let g =
        Game
          { _cactusPos = V2 200 6,
            _dinoPos = V2 20 5,
            _dinoMvmt = Normal
          }
  return g

-- Refresh game states on each tick
refresh :: Game -> Game
refresh = moveCacti

moveCacti :: Game -> Game
moveCacti g = g & cactusPos %~ f
  where
    f (V2 x y) = V2 ((x -1) `mod` 200) y

dinoJump :: Game -> Game
dinoJump = undefined