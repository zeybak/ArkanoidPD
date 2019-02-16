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

{- Update game after inputs -}
updateGameWithInput :: Event -> ArkanoidGame -> ArkanoidGame
updateGameWithInput event game = game { player = player' }
    where
        player' = handlePlayerInputs event (player game)

{- Update world object location -}
updateObjectLocation :: Float -> WorldObject -> Vector -- update ball's location in the world
updateObjectLocation seconds object = setLocation (x', y') object
    where
        x = getLocationX object
        y = getLocationY object
        (vx, vy) = getVelocity object

        x' = x + vx * seconds -- this way the ball moves smoothly by using deltaTime
        y' = y + vy * seconds

{- Check first object collision with a second object -}
checkCollision :: WorldObject -> WorldObject -> CollisionType
checkCollision firstObject secondObject
    | boundariesDistance firstObject RightBoundary secondObject LeftBoundary <= (distanceToCollide generalSettings) = RightCollision
    | boundariesDistance firstObject LeftBoundary secondObject RightBoundary <= (distanceToCollide generalSettings) = LeftCollision
    | boundariesDistance firstObject BottomBoundary secondObject TopBoundary <= (distanceToCollide generalSettings) = BottomCollision
    | boundariesDistance firstObject TopBoundary secondObject BottomBoundary <= (distanceToCollide generalSettings) = TopCollision
    | otherwise = NoCollision

{- Handle first object velocity based on collision with a second object -}
handleCollision :: WorldObject -> WorldObject -> Vector
handleCollision firstObject secondObject
    | checkCollision firstObject secondObject == RightCollision && getVelocityX firstObject > 0 = 
        sumVectors (createVector (-(getVelocityX firstObject) * 2, getVelocityY firstObject)) (createVector (getVelocity secondObject))
    | checkCollision firstObject secondObject == LeftCollision && getVelocityX firstObject < 0 = 
        sumVectors (createVector (-(getVelocityX firstObject) * 2, getVelocityY firstObject)) (createVector (getVelocity secondObject))
    | checkCollision firstObject secondObject == BottomCollision && getVelocityY firstObject < 0 = 
        sumVectors (createVector (getVelocityX firstObject, -(getVelocityY firstObject) * 2)) (createVector (getVelocity secondObject))
    | checkCollision firstObject secondObject == TopCollision && getVelocityY firstObject > 0 = 
        sumVectors (createVector (getVelocityX firstObject, -(getVelocityY firstObject) * 2)) (createVector (getVelocity secondObject))
    | otherwise = createVector (0, 0)

{- Update ball velocity based on collisions -}
updateBallVelocity :: ArkanoidGame -> Vector
updateBallVelocity game = sumVectors ballSpeed' collisionsExtraSpeed'
    where
        ballSpeed' = createVector (getVelocity (ball game))
        collisionsExtraSpeed' = sumMultipleVectors playerCollision' wallsCollision'
            where
                playerCollision' = (handleCollision (ball game) (player game))
                wallsCollision' = (map (handleCollision (ball game)) (walls game))

{- Update all game object's -}
updateGame :: Float -> ArkanoidGame -> ArkanoidGame
updateGame seconds game = game { ball = ball', player = player' }
    where
        ballObj = (ball game)
        playerObj = (player game)

        ball' = ballObj 
            { 
                location = updateObjectLocation seconds ballObj,
                velocity = updateBallVelocity game
            }
        player' = playerObj 
            { 
                location = updateObjectLocation seconds playerObj 
            }

{- Update game state -}
update :: Float -> ArkanoidGame -> ArkanoidGame
update seconds = updateGame seconds