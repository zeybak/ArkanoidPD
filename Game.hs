module Game where

import World
import Graphics.Gloss hiding (Scale)
import Graphics.Gloss.Data.ViewPort

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

{- Movement of the ball -}
ballMovement :: Float -> ArkanoidGame -> ArkanoidGame
ballMovement seconds game = game { ballLoc = newBallLoc' }
    where
        newBallLoc' = Location { xLoc = x' , yLoc = y' }
            where
                x = xLoc (ballLoc game)
                y = yLoc (ballLoc game)
                (vx, vy) = ballVel game

                x' = x + vx * seconds -- this way the ball moves smoothly
                y' = y + vy * seconds

{- Game Updates -}
fps :: Int
fps = 60

update :: ViewPort -> Float -> ArkanoidGame -> ArkanoidGame
update _ = ballMovement