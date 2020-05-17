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

        oldPosition = body ^. bodyPosition 
        newPosition = addPoints oldPosition (mulSV timePassed newVelocity)

        collision = detectMapCollision map newPosition (body ^. bodyCollisionBox)
        
        replaceIfCollision isCollision original replace = 
            original &~ do
                _1 %= (\x -> if fst isCollision then fst replace else x)
                _2 %= (\y -> if snd isCollision then snd replace else y)

        newPosition' = replaceIfCollision collision newPosition oldPosition

        collisionVelocity = mulSV collisionVelocityRate newVelocity
        newVelocity' = replaceIfCollision collision newVelocity collisionVelocity


gravityAcceleration :: Body -> Float 
gravityAcceleration body = - g * body ^. weight

detectMapCollision :: Map -> Position -> CollisionBox -> (Bool, Bool)
detectMapCollision (Map _ maxW maxH allBlocks) position collisionBox
    = isBorderCollision 
        & _1 %~ (&& isBlockCollision)
        & _2 %~ (&& isBlockCollision) 
    where 
        checkBlockCollision (Block blockPosition _ w h) 
            = detectCollision position blockPosition collisionBox (RectangleBox w h) 
        
        isBlockCollision = and (checkBlockCollision <$> allBlocks)
        isBorderCollision = detectBorderCollision (maxW, maxH) position collisionBox            

detectBorderCollision :: Position -> Position -> CollisionBox -> (Bool, Bool)
detectBorderCollision (maxW, maxH) (x, y) (RectangleBox w h) 
    = (xCollision, yCollision)
    where 
        xCollision = x < 0 || x + w > maxW
        yCollision = y < 0 || y + h > maxH

detectBorderCollision (maxW, maxH) (x, y) (CircleBox r)
    = (xCollision, yCollision)
    where
        xCollision = x - r < 0 || x + r > maxW
        yCollision = y - r < 0 || y + r > maxH

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