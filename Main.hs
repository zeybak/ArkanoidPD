module Main where

import Settings
import Game
import Render
import Graphics.Gloss hiding (Scale)

main :: IO ()
main = simulate window (backgroundColor graphicSettings) (fps generalSettings) initialGameState render update