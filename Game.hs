module Game where

import World
import Graphics.Gloss hiding (Scale)

{- How the state of the game is represented -}
data ArkanoidGame = Game
    {
        ballLoc :: Location,
        ballVel :: (Float, Float),
        playerLoc :: Location,
        playerHorizontalAxis :: Float
    }

{- The very first state of the game -}
initialGameState :: ArkanoidGame
initialGameState = Game
    {
        ballLoc = Location { xLoc = 0, yLoc = 0 },
        ballVel = (0, -30),
        playerLoc = Location { xLoc = 0, yLoc = -130 },
        playerHorizontalAxis = 0
    }