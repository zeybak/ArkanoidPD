module Main where

import Settings
import Game
import Render
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

main :: IO ()
main = play window (backgroundColor graphicSettings) (fps generalSettings) initialGameState render updateGameWithInput update