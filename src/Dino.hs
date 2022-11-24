{-# LANGUAGE TemplateHaskell #-}
module Dino ( Game, step, cactusPos, dinoPos, initGame ) where

import Lens.Micro.TH (makeLenses)
import Lens.Micro ( (&), (%~) ) 
import Linear.V2 (V2(..))

data Game = Game
    {   _cactusPos :: Pos,  -- ^  position of cactus (maybe a list later)
        _dinoPos :: Pos -- ^ position of dino
    } deriving (Show)

type Pos = V2 Int

makeLenses ''Game

initGame :: IO Game
initGame = do
  let g =
        Game
          { _cactusPos = V2 200 6, 
            _dinoPos = V2 20 5
          }
  return g

step :: Game -> Game
step g = g & cactusPos %~ f
    where f (V2 x y) = V2 ((x-1) `mod` 200) y

