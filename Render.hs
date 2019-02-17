module Render where

import Settings
import World
import Game
import Graphics.Gloss hiding (Scale)

{- Window display -}
window :: Display
window = InWindow "UBP PD: Arkanoid" (getVectorInt (windowSize generalSettings)) (15, 15)

{- Draw graphic objects as a picture to be rendered -}
drawBall :: WorldObject -> Picture
drawBall object = 
    translate (getLocationX object) (getLocationY object) $ -- move object in world
    color (getColor object) $ -- set object color
    circleSolid (getScaleX object) -- draw a solid circle with defined radius
drawBlock :: WorldObject -> Picture
drawBlock object
    | (enabled object) == False = blank
    | (enabled object) == True = translate (getLocationX object) (getLocationY object) $
        color (getColor object) $
        rectangleSolid (getScaleX object) (getScaleY object)

{- Render all pictures in the game -}
render :: ArkanoidGame -> Picture
render game = pictures
    [
        ballRender,
        playerRender,
        wallsRender,
        bricksRender
    ]
    where
        ballRender = drawBall (ball game)
        playerRender = drawBlock (player game)
        wallsRender = pictures (map drawBlock (walls game)) -- iterate over all walls in the game using drawBlock
        bricksRender = pictures (map drawBlock (bricks game))