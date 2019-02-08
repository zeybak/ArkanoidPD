module Main where

import Game
import Render
import Graphics.Gloss hiding (Scale)

main :: IO ()
main = simulate window (backgroundColor defaultGraphicSettings) fps initialGameState render update