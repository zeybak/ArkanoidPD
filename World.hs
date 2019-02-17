module World where

import Graphics.Gloss (Color)

{- Represent Objects in world data -}
data Vector = Vector
    {
        x :: Float,
        y :: Float
    }
data WorldObject = WorldObject
    {
        location :: Vector, -- Where is the object located on the world
        scale :: Vector, -- What's the scale of the object on the world
        velocity :: Vector, -- Current object's velocity on the world
        objColor :: Color, -- Object's color
        enabled :: Bool
    }
data CollisionType = NoCollision | LeftCollision | RightCollision | TopCollision | BottomCollision deriving (Eq)
data BoundaryType = LeftBoundary | RightBoundary | TopBoundary | BottomBoundary deriving (Eq)

{- Clamp value -}
clampValue :: Float -> Float -> Float -> Float
clampValue value min max
    | value < min = min
    | value > max = max
    | otherwise = value

{- Vector handling functions -}
createVector :: (Float, Float) -> Vector
createVector (x', y') = Vector { x = x' , y = y' }
setVectorX :: Float -> Vector -> Vector
setVectorX value vector = vector { x = value }
setVectorY :: Float -> Vector -> Vector
setVectorY value vector = vector { y = value }
setVector :: (Float, Float) -> Vector -> Vector
setVector (x, y) vector = setVectorX x (setVectorY y vector)
getVectorX :: Vector -> Float
getVectorX vector = x vector;
getVectorY :: Vector -> Float
getVectorY vector = y vector;
getVector :: Vector -> (Float, Float)
getVector vector = (getVectorX vector, getVectorY vector)
getVectorInt :: Vector -> (Int, Int)
getVectorInt vector = (round (getVectorX vector), round (getVectorY vector))
sumVectors :: Vector -> Vector -> Vector
sumVectors a b = createVector (getVectorX a + getVectorX b, getVectorY a + getVectorY b)
sumMultipleVectors :: Vector -> [Vector] -> Vector
sumMulipleVectors accVector [] = accVector
sumMultipleVectors accVector (x:[]) = sumVectors accVector x
sumMultipleVectors accVector (x:xs) = sumMultipleVectors func' xs
    where
        func' = sumVectors accVector x
clampVectorElements :: Vector -> Float -> Float -> Vector
clampVectorElements vector min max = createVector (clampValue (getVectorX vector) min max, clampValue (getVectorY vector) min max)

{- Location handling functions -}
setLocation :: (Float, Float) -> WorldObject -> Vector
setLocation (x, y) object = setVector (x, y) (location object)
getLocationX :: WorldObject -> Float
getLocationX object = getVectorX (location object)
getLocationY :: WorldObject -> Float
getLocationY object = getVectorY (location object)
getLocation :: WorldObject -> (Float, Float)
getLocation object = (getLocationX object, getLocationY object)

{- Scale handling functions -}
setScale :: (Float, Float) -> WorldObject -> Vector
setScale (x, y) object = setVector (x, y) (location object)
getScaleX :: WorldObject -> Float
getScaleX object = getVectorX (scale object)
getScaleY :: WorldObject -> Float
getScaleY object = getVectorY (scale object)
getScale :: WorldObject -> (Float, Float)
getScale object = (getScaleX object, getScaleY object)

{- Velocity handling functions -}
setVelocity :: (Float, Float) -> WorldObject -> Vector
setVelocity (x, y) object = setVector (x, y) (location object)
getVelocityX :: WorldObject -> Float
getVelocityX object = getVectorX (velocity object)
getVelocityY :: WorldObject -> Float
getVelocityY object = getVectorY (velocity object)
getVelocity :: WorldObject -> (Float, Float)
getVelocity object = (getVelocityX object, getVelocityY object)

{- Color handling functions -}
setColor :: Color -> WorldObject -> WorldObject
setColor newColor object = object { objColor = newColor }
getColor :: WorldObject -> Color
getColor object = objColor object

{- Objects creation functions -}
createObject :: (Float, Float) -> (Float, Float) -> (Float, Float) -> Color -> WorldObject
createObject (xLoc, yLoc) (xScale, yScale) (xVel, yVel) color = WorldObject
                                                                    {
                                                                        location = createVector(xLoc, yLoc),
                                                                        scale = createVector(xScale, yScale),
                                                                        velocity = createVector(xVel, yVel),
                                                                        objColor = color,
                                                                        enabled = True
                                                                    }
createBall :: (Float, Float) -> Float -> (Float, Float) -> Color -> WorldObject
createBall (xLoc, yLoc) scale (xVel, yVel) color = createObject (xLoc, yLoc) (scale, scale) (xVel, yVel) color
createBlock :: (Float, Float) -> (Float, Float) -> (Float, Float) -> Color -> WorldObject
createBlock (xLoc, yLoc) (xScale, yScale) (xVel, yVel) color = createObject (xLoc, yLoc) (xScale, yScale) (xVel, yVel) color

{- Object's boundaries -}
boundaryValue :: WorldObject -> BoundaryType -> Float
boundaryValue object LeftBoundary = getLocationX object - getScaleX object
boundaryValue object RightBoundary = getLocationX object + getScaleX object
boundaryValue object BottomBoundary = getLocationY object - getScaleY object
boundaryValue object TopBoundary = getLocationY object + getScaleY object

{- Get distance between boundaries -}
boundariesDistance :: WorldObject -> BoundaryType -> WorldObject -> BoundaryType -> Float
boundariesDistance firstObject firstBoundary secondObject secondBoundary = abs ((boundaryValue firstObject firstBoundary) - (boundaryValue secondObject secondBoundary))

{- Check if an object is in another object's boundaries -}
isObjectInBoundaries :: Float -> WorldObject -> WorldObject -> Bool
isObjectInBoundaries maxDist firstObject secondObject = insideHorizontal && insideVertical
    where
        insideHorizontal = abs (getLocationX firstObject - getLocationX secondObject) <= getScaleX firstObject + getScaleX secondObject + maxDist
        insideVertical = abs (getLocationY firstObject - getLocationY secondObject) <= getScaleY firstObject + getScaleY secondObject + maxDist