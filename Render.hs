module Render where

import World
import Game
import Graphics.Gloss hiding (Scale)

{- Window settings -}
window :: Display
window = InWindow "ArkanoidPD" (300, 300) (15, 15)

{- Graphic settings -}
data ArkanoidGraphicSettings = ArkanoidGraphicSettings
    {
        backgroundColor :: Color,
        ballColor :: Color,
        ballRadius :: Float,
        paddleColor :: Color,
        paddleSize :: Scale,
        wallColor :: Color,
        wallSize :: Scale
    }
defaultGraphicSettings :: ArkanoidGraphicSettings
defaultGraphicSettings = ArkanoidGraphicSettings
    {
        backgroundColor = black,
        ballColor = white,
        ballRadius = 5,
        paddleColor = white,
        paddleSize = Scale { xScale = 50, yScale = 10 },
        wallColor = dark (dark white),
        wallSize = Scale { xScale = 300, yScale = 10 }
    }

{- Graphic Objects -}
ball :: Location -> Picture
ball location = translate (xLoc location) (yLoc location) $ 
    color (ballColor defaultGraphicSettings) $ 
    circleSolid (ballRadius defaultGraphicSettings)
block :: Location -> Picture
block location = translate (xLoc location) (yLoc location) $ 
    color (paddleColor defaultGraphicSettings) $ 
    rectangleSolid (xScale (paddleSize defaultGraphicSettings)) (yScale (paddleSize defaultGraphicSettings))
horizontalWall :: Location -> Picture
horizontalWall location = translate (xLoc location) (yLoc location) $ 
    color (wallColor defaultGraphicSettings) $ 
    rectangleSolid (xScale (wallSize defaultGraphicSettings)) (yScale (wallSize defaultGraphicSettings))
verticalWall :: Location -> Picture
verticalWall location = translate (xLoc location) (yLoc location) $ 
    color (wallColor defaultGraphicSettings) $ 
    rectangleSolid (yScale (wallSize defaultGraphicSettings)) (xScale (wallSize defaultGraphicSettings))

{- How the render is going to be displayed as a Picture every simulation -}
render :: ArkanoidGame -> Picture
render game = pictures
    [
        ballRender,
        playerRender,
        wallsRender
    ]
    where
        ballRender = ball (ballLoc game)
        playerRender = block (playerLoc game)
        wallsRender = pictures  [
                                    horizontalWall Location { xLoc = 0, yLoc = -150 }, 
                                    horizontalWall Location { xLoc = 0, yLoc = 150 },
                                    verticalWall Location { xLoc = -150, yLoc = 0 },
                                    verticalWall Location { xLoc = 150, yLoc = 0 }
                                ]