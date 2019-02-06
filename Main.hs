module Main where

import Game
import Render
import Graphics.Gloss hiding (Scale)

main :: IO ()
main = display window (backgroundColor defaultGraphicSettings) (render initialGameState)