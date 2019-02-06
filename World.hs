module World where

{- Represent Objects in world data -}
data Location = Location -- Where is the object located on the world
    {
        xLoc :: Float,
        yLoc :: Float
    }
data Scale = Scale -- What's the scale of the object on the world
    {
        xScale :: Float,
        yScale :: Float
    }