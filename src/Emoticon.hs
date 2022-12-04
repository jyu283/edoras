{-# OPTIONS_GHC -Wno-type-defaults #-}

module Emoticon
  ( dino1Widget,
    cactus1Widget,
    cactus2Widget, 
    ground1Widget,
    dino1DuckWidget,
  )
where

import Brick
import Data.Either
import Text.Parsec
import Text.Parsec.String

-----------------24X10 dino1----------------------------------
--             ##.#########
--             ############
--             ######////**
-- ,          ,####
-- ##     #########(((
-- ################
--  ##############/
--      #########
--       ### .##
--       #     #

dino1Str :: String
dino1Str = "            ##.#########\n            ############\n            ######////**\n,          ,####        \n##     #########(((     \n################        \n ##############/        \n     #########          \n      ### .##           \n      #     #           "

dino1Widget :: Widget n
dino1Widget = getEmoticonWidget dino1Str

----------------- 33X5 dino1----------------------------------
-- #######################.#########
--     #############################
--       #####################////**
--        #### .##     (((
--        #      #
dino1DuckStr :: String
dino1DuckStr = "#######################.#########\n    #############################\n      #####################////**\n       #### .##     (((          \n       #      #                  "

dino1DuckWidget ::  Widget n
dino1DuckWidget = getEmoticonWidget dino1DuckStr
-----------------15X9 cactus1----------------------------------
--      (((((.
--      ((,(((
--      (((((( (##
--      *((((/ ###
--  ##   ((((####
-- .###  ((((
--  #####(.((
--       ((((
--       ((((

cactus1Str :: String
cactus1Str = "     (((((.    \n     ((,(((    \n     (((((( (##\n     *((((/ ###\n ##   ((((#### \n.###  ((((     \n #####(.((     \n      ((((     \n      ((((     "

cactus1Widget :: Widget n
cactus1Widget = getEmoticonWidget cactus1Str

-----------------29X11 cactus2----------------------------------
--      .##(           (##.      
--      (###.      ,  .###(      
--      (###.     ### .###(      
-- .##, (###. ##. ### .###( *##, 
-- .##* (###. ##, ### .###( *##, 
-- .##* (###. ##,  #######( *##, 
-- .##(*(###/*##      .####*/##  
--   /######.         .###(..    
--      (###.         .###(      
--      (###.         .###(      
--      (###.         .###(      

cactus2Str :: String
cactus2Str = "     .##(           (##.     \n     (###.      ,  .###(     \n     (###.     ### .###(     \n.##, (###. ##. ### .###( *##,\n.##* (###. ##, ### .###( *##,\n.##* (###. ##,  #######( *##,\n.##(*(###/*##      .####*/## \n  /######.         .###(..   \n     (###.         .###(     \n     (###.         .###(     \n     (###.         .###(     "

cactus2Widget :: Widget n
cactus2Widget = getEmoticonWidget cactus2Str
    
ground1Widget :: Widget n
ground1Widget = vBox [row | _ <- [1 .. 4]]
  where
    row = str ['-' | _ <- [1 .. 200]]

-- a function that replaces every character a with character b in a given string, can be used to set backgroud to " " if the original emoticon has non empty backgroud
replaceChar :: Char -> Char -> String -> String
replaceChar a b = map replace
  where
    replace x
      | x == a = b
      | otherwise = x

parseFromString :: Parser a -> String -> Either ParseError a
parseFromString p s = runParser p () "DUMMY" s

-- break a multi-line string into a list of single line strings
multiLineP :: Parser [String]
multiLineP =
  try
    ( do
        x <- many1 (noneOf ['\n'])
        _ <- many (char '\n')
        xs <- multiLineP
        return (x : xs)
    )
    <|> return []

-- parse a string to a list of widgets, skip white spaces " "
oneLineWidgetP :: Parser [Widget n]
oneLineWidgetP =
  try
    ( do
        whiteSpaces <- many (char ' ')
        s <- many1 (noneOf [' '])
        let m = length whiteSpaces
        xs <- oneLineWidgetP
        let x = translateBy (Location (m, 0)) (str s)
        return (x : xs)
    )
    <|> return []

getEmoticonWidget :: String -> Widget n
getEmoticonWidget emoStr = vBox $ map hBox widgetss
  where
    widgetss = map (\s -> fromRight [] (parseFromString oneLineWidgetP s) ++ [translateBy (Location (10000, 0)) (str " ")]) emoStrs
      where
        -- To ensure that the backgroud of each hBox widget is transparent, at the end of each line, a sufficiently far away "space widget" (str " ") is added. Otherwise, vBox will automatically pad on the right (probably with " " ?) to align all hBoxes, making the right side of the overall widget not transparent.
        emoStrs = fromRight [] (parseFromString multiLineP emoStr)
