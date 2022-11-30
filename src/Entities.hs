{-# LANGUAGE TemplateHaskell #-}

module Entities (Game, refresh, cactusPos, dinoPos, initGame, dinoJump) where

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
    -- | movement of dino
    _dinoMvmt :: Movement
  }
  deriving (Show)

makeLenses ''Game -- What's this for?

initGame :: IO Game
initGame = do
  let g =
        Game
          { _cactusPos = V2 200 18,
            _dinoPos = V2 20 17,
            _dinoMvmt = Normal
          }
  return g

-- Refresh game states on each tick
refresh :: Game -> Game
refresh = refreshCactus . refreshDino

refreshCactus :: Game -> Game
refreshCactus g = g & cactusPos %~ f
  where
    f (V2 x y) = V2 ((x -1) `mod` 200) y

refreshDino :: Game -> Game
refreshDino g = case g ^. dinoMvmt of
  Jumping -> if getDinoHeight g == 2 then setDinoMvt g Falling else moveDino g (-1)
  Falling -> if getDinoHeight g == 17 then setDinoMvt g Normal else moveDino g 1
  _other -> g

moveDino :: Game -> Int -> Game
moveDino g delta = g & dinoPos .~ V2 20 (getDinoHeight g + delta)

getDinoHeight :: Game -> Int
getDinoHeight g = getV2y (g ^. dinoPos)

getV2y :: V2 Int -> Int
getV2y (V2 _ y) = y

setDinoMvt :: Game -> Movement -> Game
setDinoMvt g mvt = g & dinoMvmt .~ mvt

dinoJump :: Game -> Game
dinoJump g = case g ^. dinoMvmt of
  Normal -> setDinoMvt g Jumping
  Ducking -> setDinoMvt g Jumping
  _other -> g