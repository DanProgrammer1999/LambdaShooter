module Physics where

import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Vector
import Control.Lens

import CommonData 
import Constants


limitVelocity :: Velocity -> Velocity
limitVelocity (x, y) = (newX, newY)
    where
        newX = min x maxMovementSpeed
        newY = min y maxMovementSpeed

updateBody :: Float -> Body -> Body
updateBody timePassed body = body &~
    do 
        bodyAcceleration .= newAcceleration'
        bodyVelocity     .= newVelocity
        bodyPosition     .= newPosition
    where
        oldAcceleration = body ^. bodyAcceleration
        newAcceleration = oldAcceleration & _1 %~ (\a -> a - timePassed * deceleration a)
        newAcceleration' = 
            if fst newVelocity == maxMovementSpeed 
            then newAcceleration & _1 .~ 0
            else newAcceleration 

        newVelocity = limited
            where
                addedVelocity = addPoints (body ^. bodyVelocity) newAcceleration

                friction = timePassed * calcFriction (fst addedVelocity)
                withFriction = addedVelocity & _1 -~ friction
                limited = limitVelocity withFriction

        newPosition = addPoints (body ^. bodyPosition) (mulSV timePassed newVelocity)

calcFriction :: Float -> Float
calcFriction vx 
    | vx == 0   = 0
    | otherwise = signum vx * frictionRate

deceleration :: Float -> Float
deceleration ax 
    | ax == 0   = 0
    | otherwise = signum ax * decelerationRate

detectCollision :: Position -> Position -> CollisionBox -> CollisionBox -> Bool
detectCollision
    (x1, y1)
    (x2, y2)
    (RectangleBox width1 height1)
    (RectangleBox width2 height2)
    =  x1 < x2 + width2
    && x1 + width1 > x2
    && y1 < y2 + height2
    && y1 + height1 > y2

detectCollision c1 c2 (CircleBox r1) (CircleBox r2)
    = distance c1 c2 < r1 + r2

detectCollision
    (xr, yr)
    (xc, yc)
    (RectangleBox width height)
    (CircleBox r)
    = distance (testX, testY) (xc, yc) <= r
    where
        testX = if xc < xr then xr else xr + width
        testY = if yc < yr then yr else yr + height

detectCollision p1 p2 c@(CircleBox _) r@(RectangleBox _ _)
    = detectCollision p2 p1 r c

addPoints :: Point -> Point -> Point
addPoints (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

distance :: Point -> Point -> Float
distance (x1, y1) (x2, y2) = sqrt $ (x1 - x2)^2 + (y1 - y2)^2