module Game where

import Settings
import Input
import World
import Graphics.Gloss hiding (Scale, Vector)
import Graphics.Gloss.Interface.Pure.Game hiding (Vector)

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
                    createBlock (0, (getVectorX (windowSize generalSettings)) * 0.5) (horizontalWallScale generalSettings) (0, 0) (wallColor graphicSettings),
                    createBlock (-(getVectorX (windowSize generalSettings)) * 0.5, 0) (verticalWallScale generalSettings) (0, 0) (wallColor graphicSettings),
                    createBlock ((getVectorX (windowSize generalSettings)) * 0.5, 0) (verticalWallScale generalSettings) (0, 0) (wallColor graphicSettings)
                ]
    }

{- Update world object location -}
updateObjectLocation :: Float -> WorldObject -> Vector -- update ball's location in the world
updateObjectLocation seconds object = setLocation (x', y') object
    where
        x = getLocationX object
        y = getLocationY object
        (vx, vy) = getVelocity object

        x' = x + vx * seconds -- this way the ball moves smoothly by using deltaTime
        y' = y + vy * seconds

{- Update world object state -}
updateObject :: Float -> WorldObject -> WorldObject
updateObject seconds object = object { location = updateObjectLocation seconds object }

{- Update game after inputs -}
updateGameWithInput :: Event -> ArkanoidGame -> ArkanoidGame
updateGameWithInput event game = game { player = player' }
    where
        player' = handlePlayerInputs event (player game)

{- Update all game object's -}
updateGame :: Float -> ArkanoidGame -> ArkanoidGame
updateGame seconds game = game { ball = ball', player = player' }
    where
        ball' = updateObject seconds (ball game)
        player' = updateObject seconds (player game)

{- Update game state -}
update :: Float -> ArkanoidGame -> ArkanoidGame
update seconds = updateGame seconds