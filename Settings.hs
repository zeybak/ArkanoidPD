module Settings where

import World
import Graphics.Gloss hiding (Scale)

{- All game's general settings -}
data ArkanoidGeneralSettings = ArkanoidGeneralSettings
    {
        fps :: Int,
        ballRadius :: Float,
        playerScale :: (Float, Float),
        horizontalWallScale :: (Float, Float),
        verticalWallScale :: (Float, Float)
    }
generalSettings :: ArkanoidGeneralSettings
generalSettings = ArkanoidGeneralSettings
    {
        fps = 60,
        ballRadius = 5,
        playerScale = (50, 10),
        horizontalWallScale = (300, 10),
        verticalWallScale = (10, 300)
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