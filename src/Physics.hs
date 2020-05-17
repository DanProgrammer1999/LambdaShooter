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

updateBody :: Float -> Map -> Body -> Body
updateBody timePassed map body = body &~
    do 
        bodyPosition .= newPosition'
        bodyVelocity .= newVelocity'
    where
        oldVelocity = body ^. bodyVelocity 
        newVelocity = oldVelocity & _2 +~ timePassed * gravityAcceleration body

        (oldX, oldY) = body ^. bodyPosition 
        (newX, newY) = (oldX + timePassed * fst newVelocity, oldY + timePassed * snd newVelocity)
        (xUpdate, yUpdate) = ((newX, oldY), (oldX, newY))

        [xCollision, yCollision] = fmap (detectMapCollision map (body ^. bodyCollisionBox)) [xUpdate, yUpdate]

        replaceIfCollision isCollision original replace = 
            original &~ do
                _1 %= (\x -> if fst isCollision then fst replace else x)
                _2 %= (\y -> if snd isCollision then snd replace else y)

        newPosition' = replaceIfCollision (xCollision, yCollision) (newX, newY) (oldX, oldY)

        collisionVelocity = mulSV collisionVelocityRate newVelocity
        newVelocity' = replaceIfCollision (xCollision, yCollision) newVelocity collisionVelocity


gravityAcceleration :: Body -> Float 
gravityAcceleration body = - g * body ^. weight

detectMapCollision :: Map -> CollisionBox -> Position -> Bool
detectMapCollision (Map _ maxW maxH allBlocks) collisionBox position
    = and (checkBlockCollision <$> allBlocks)
    where 
        checkBlockCollision (Block blockPosition _ w h) 
            = detectCollision position blockPosition collisionBox (RectangleBox w h) 
        
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