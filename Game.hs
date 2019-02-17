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
        walls :: [WorldObject],
        bricks :: [WorldObject]
    }

{- The very first state of the game -}
initialGameState :: ArkanoidGame
initialGameState = Game
    {
        ball = createBall (0, 0) (ballRadius generalSettings) (0, -(ballSpeed generalSettings)) (ballColor graphicSettings),
        player = createBlock (0, -130) (playerScale generalSettings) (0, 0) (playerColor graphicSettings),
        walls = [
                    createBlock (0, (getVectorX (windowSize generalSettings)) * 0.5) (horizontalWallScale generalSettings) (0, 0) (wallColor graphicSettings),
                    createBlock (-(getVectorX (windowSize generalSettings)) * 0.5, 0) (verticalWallScale generalSettings) (0, 0) (wallColor graphicSettings),
                    createBlock ((getVectorX (windowSize generalSettings)) * 0.5, 0) (verticalWallScale generalSettings) (0, 0) (wallColor graphicSettings)
                ],
        bricks = [
                    createBlock (-105, 110) (bricksScale generalSettings) (0, 0) (bricksColor graphicSettings),
                    createBlock (-105, 90) (bricksScale generalSettings) (0, 0) (bricksColor graphicSettings),
                    createBlock (-105, 70) (bricksScale generalSettings) (0, 0) (bricksColor graphicSettings),
                    createBlock (-105, 50) (bricksScale generalSettings) (0, 0) (bricksColor graphicSettings),
                    createBlock (-35, 110) (bricksScale generalSettings) (0, 0) (bricksColor graphicSettings),
                    createBlock (-35, 90) (bricksScale generalSettings) (0, 0) (bricksColor graphicSettings),
                    createBlock (-35, 70) (bricksScale generalSettings) (0, 0) (bricksColor graphicSettings),
                    createBlock (-35, 50) (bricksScale generalSettings) (0, 0) (bricksColor graphicSettings),
                    createBlock (35, 110) (bricksScale generalSettings) (0, 0) (bricksColor graphicSettings),
                    createBlock (35, 90) (bricksScale generalSettings) (0, 0) (bricksColor graphicSettings),
                    createBlock (35, 70) (bricksScale generalSettings) (0, 0) (bricksColor graphicSettings),
                    createBlock (35, 50) (bricksScale generalSettings) (0, 0) (bricksColor graphicSettings),
                    createBlock (105, 110) (bricksScale generalSettings) (0, 0) (bricksColor graphicSettings),
                    createBlock (105, 90) (bricksScale generalSettings) (0, 0) (bricksColor graphicSettings),
                    createBlock (105, 70) (bricksScale generalSettings) (0, 0) (bricksColor graphicSettings),
                    createBlock (105, 50) (bricksScale generalSettings) (0, 0) (bricksColor graphicSettings)
                ]
    }

{- How many bricks haven't been destroyed -}
bricksEnabledAmount :: Int -> [WorldObject] -> Int
bricksEnabledAmount accValue [] = accValue
bricksEnabledAmount accValue (x:[])
    | (enabled x) == True = accValue + 1
    | otherwise = accValue
bricksEnabledAmount accValue (x:xs)
    | (enabled x) == True = bricksEnabledAmount (accValue + 1) xs
    | otherwise = bricksEnabledAmount accValue xs

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

{- Check collision type based on velocity -}
checkCollisionType :: WorldObject -> WorldObject -> CollisionType
checkCollisionType firstObject secondObject
    | boundariesDistance firstObject RightBoundary secondObject LeftBoundary <= (distanceToCollide generalSettings) && getVelocityX firstObject > 0 = RightCollision
    | boundariesDistance firstObject LeftBoundary secondObject RightBoundary <= (distanceToCollide generalSettings) && getVelocityX firstObject < 0 = LeftCollision
    | boundariesDistance firstObject TopBoundary secondObject BottomBoundary <= (distanceToCollide generalSettings) && getVelocityY firstObject > 0 = TopCollision
    | boundariesDistance firstObject BottomBoundary secondObject TopBoundary <= (distanceToCollide generalSettings) && getVelocityY firstObject < 0 = BottomCollision
    | otherwise = NoCollision

{- Check first object collision with a second object -}
checkCollision :: WorldObject -> WorldObject -> CollisionType
checkCollision firstObject secondObject
    | (enabled secondObject) == False = NoCollision
    | isObjectInBoundaries (distanceToCollide generalSettings) firstObject secondObject == False = NoCollision
    | isObjectInBoundaries (distanceToCollide generalSettings) firstObject secondObject == True = collisionType
        where
            collisionType = checkCollisionType firstObject secondObject

{- Handle first object velocity based on collision with a second object -}
handleCollision :: WorldObject -> WorldObject -> Vector
handleCollision firstObject secondObject
    | checkCollision firstObject secondObject == RightCollision = 
        sumVectors (createVector (-(getVelocityX firstObject) * 2, 0)) (createVector (getVelocity secondObject))
    | checkCollision firstObject secondObject == LeftCollision = 
        sumVectors (createVector (-(getVelocityX firstObject) * 2, 0)) (createVector (getVelocity secondObject))
    | checkCollision firstObject secondObject == BottomCollision = 
        sumVectors (createVector (0, -(getVelocityY firstObject) * 2)) (createVector (getVelocity secondObject))
    | checkCollision firstObject secondObject == TopCollision = 
        sumVectors (createVector (0, -(getVelocityY firstObject) * 2)) (createVector (getVelocity secondObject))
    | otherwise = createVector (0, 0)

{- Check if ball hitted a brick and therefore disable it -}
handleBrickCollision :: WorldObject -> WorldObject -> WorldObject
handleBrickCollision ball brick
    | (enabled brick) == False = brick
    | isObjectInBoundaries (distanceToCollide generalSettings) ball brick == True = brick { enabled = False }
    | otherwise = brick

{- Update ball velocity based on collisions -}
updateBallVelocity :: ArkanoidGame -> Vector
updateBallVelocity game = sumVectors ballSpeed' collisionsExtraSpeed'
    where
        ballSpeed' = createVector (getVelocity (ball game))
        collisionsExtraSpeed' = sumMultipleVectors (sumMultipleVectors playerCollision' wallsCollision') bricksCollision'
            where
                playerCollision' = (handleCollision (ball game) (player game))
                wallsCollision' = (map (handleCollision (ball game)) (walls game))
                bricksCollision' = (map (handleCollision (ball game)) (bricks game))

{- Update all game object's -}
updateGame :: Float -> ArkanoidGame -> ArkanoidGame
updateGame seconds game = game { ball = ball', player = player', bricks = bricks' }
    where
        ballObj = (ball game)
        playerObj = (player game)

        ball' = ballObj 
            { 
                location = updateObjectLocation seconds ballObj,
                velocity = clampVectorElements (updateBallVelocity game) (-(ballSpeed generalSettings)) (ballSpeed generalSettings)
            }
        player' = playerObj 
            { 
                location = updateObjectLocation seconds playerObj 
            }
        bricks' = map (handleBrickCollision (ball game)) (bricks game)

{- Check if the game should restart, win or lose -}
checkGameState :: ArkanoidGame -> ArkanoidGame
checkGameState game
    | bricksEnabledAmount 0 (bricks game) <= 0 = initialGameState
    | getLocationY (ball game) < -(getVectorY (windowSize generalSettings)) * 0.5 = initialGameState
    | otherwise = game

{- Update game state -}
update :: Float -> ArkanoidGame -> ArkanoidGame
update seconds game = checkGameState (updateGame seconds game)