module Main (main) where

import Brick
import DinoApp (app, initGame)
import Lib

main :: IO ()
main = do
  g <- initGame
  defaultMain app g
  return ()
