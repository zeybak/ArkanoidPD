module Settings where

import World
import Graphics.Gloss hiding (Scale, Vector)

{- All game's general settings -}
data ArkanoidGeneralSettings = ArkanoidGeneralSettings
    {
        fps :: Int,
        windowSize :: Vector,
        ballRadius :: Float,
        ballSpeed :: Float,
        playerScale :: (Float, Float),
        playerSpeed :: Float,
        horizontalWallScale :: (Float, Float),
        verticalWallScale :: (Float, Float),
        distanceToCollide :: Float
    }
generalSettings :: ArkanoidGeneralSettings
generalSettings = ArkanoidGeneralSettings
    {
        fps = 60,
        windowSize = createVector(300, 300),
        ballRadius = 5,
        ballSpeed = -30,
        playerScale = (50, 10),
        playerSpeed = 60,
        horizontalWallScale = (300, 10),
        verticalWallScale = (10, 300),
        distanceToCollide = 1
    }

{- All game's graphic settings -}
data ArkanoidGraphicSettings = ArkanoidGraphicSettings
    {
        backgroundColor :: Color,
        ballColor :: Color,
        playerColor :: Color,
        wallColor :: Color
    }
graphicSettings :: ArkanoidGraphicSettings
graphicSettings = ArkanoidGraphicSettings
    {
        backgroundColor = black,
        ballColor = white,
        playerColor = white,
        wallColor = dark (dark white)
    }