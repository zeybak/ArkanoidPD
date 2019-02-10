module Game where

import Settings
import World
import Graphics.Gloss hiding (Scale, Vector)
import Graphics.Gloss.Data.ViewPort

{- How the state of the game is represented -}
data ArkanoidGame = Game
    {
        ball :: WorldObject,
        player :: WorldObject,
        walls :: [WorldObject]
    }

{- The very first state of the game -}
initialGameState :: ArkanoidGame
initialGameState = Game
    {
        ball = createBall (0, 0) (ballRadius generalSettings) (0, -30) (ballColor graphicSettings),
        player = createBlock (0, -130) (playerScale generalSettings) (0, 0) (playerColor graphicSettings),
        walls = [
                    createBlock (0, -150) (horizontalWallScale generalSettings) (0, 0) (wallColor graphicSettings),
                    createBlock (0, 150) (horizontalWallScale generalSettings) (0, 0) (wallColor graphicSettings),
                    createBlock (-150, 0) (verticalWallScale generalSettings) (0, 0) (wallColor graphicSettings),
                    createBlock (150, 0) (verticalWallScale generalSettings) (0, 0) (wallColor graphicSettings)
                ]
    }

{- Update ball state -}
updateBall :: Float -> WorldObject -> WorldObject
updateBall seconds object = object { location = location' }
    where
        location' = updateBallLocation seconds object
updateBallLocation :: Float -> WorldObject -> Vector -- update ball's location in the world
updateBallLocation seconds object = setLocation (x', y') object
    where
        x = getLocationX object
        y = getLocationY object
        (vx, vy) = getVelocity object

        x' = x + vx * seconds -- this way the ball moves smoothly by using deltaTime
        y' = y + vy * seconds

{- Update all game object's -}
updateGame :: Float -> ArkanoidGame -> ArkanoidGame
updateGame seconds game = game { ball = ball' }
    where
        ball' = updateBall seconds (ball game)

{- Update game state -}
update :: ViewPort -> Float -> ArkanoidGame -> ArkanoidGame
update _ = updateGame