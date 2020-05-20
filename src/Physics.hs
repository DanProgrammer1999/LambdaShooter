module Physics where

import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Vector
import Control.Lens

import CommonData 
import Constants


updateBody :: Float -> World -> Body -> Body
updateBody timePassed world body = body &~
    do 
        bodyPosition .= newPosition'
        bodyVelocity .= newVelocity'
        collisionHappened .= (xCollision || yCollision)
    where
        oldVelocity = body ^. bodyVelocity 
        newVelocity = oldVelocity & _2 +~ timePassed * gravityAcceleration body

        oldPosition@(oldX, oldY) = body ^. bodyPosition 
        (newX, newY) = addPoints oldPosition newVelocity
        (xUpdate, yUpdate) = (oldPosition & _1 .~ newX, oldPosition & _2 .~ newY)

        collisionBox = body ^. bodyCollisionBox
        xCollision = detectMapCollision (world ^. worldMap) collisionBox xUpdate
        yCollision = detectMapCollision (world ^. worldMap) collisionBox yUpdate

        replaceIfCollision isCollision original replace = (x, y)
            where
                x = if fst isCollision then fst replace else fst original
                y = if snd isCollision then snd replace else snd original

        newPosition' = replaceIfCollision (xCollision, yCollision) (newX, newY) oldPosition

        collisionVelocity = mulSV collisionVelocityRate newVelocity
        newVelocity' = replaceIfCollision (xCollision, yCollision) newVelocity collisionVelocity

isOutOfBounds :: Body -> Bool
isOutOfBounds (Body (x, y) _ _ _ _) 
    = abs x > worldBoundary || abs y > worldBoundary

gravityAcceleration :: Body -> Float
gravityAcceleration body = - fallAcceleration * body ^. weight

detectEntitiesCollision :: [Entity] -> Body -> Bool
detectEntitiesCollision entities body = or $ checkEntityCollision <$> entities
    where
        checkEntityCollision (Entity entityBody' _ _ _) 
            = detectCollision 
                (body ^. bodyPosition) 
                (entityBody' ^. bodyPosition) 
                (body ^. bodyCollisionBox)
                (entityBody' ^. bodyCollisionBox)

detectMapCollision :: Map -> CollisionBox -> Position -> Bool
detectMapCollision (Map _ maxW maxH allBlocks) collisionBox position
    = or (checkBlockCollision <$> allBlocks)
    where 
        checkBlockCollision (Block blockPosition _ w h) 
            = detectCollision position blockPosition collisionBox (RectangleBox w h) 
        
detectCollision :: Position -> Position -> CollisionBox -> CollisionBox -> Bool
detectCollision
    (xCentre1, yCentre1)
    (xCentre2, yCentre2)
    (RectangleBox width1 height1)
    (RectangleBox width2 height2)
    =  x1 < x2 + width2
    && x1 + width1 > x2
    && y1 < y2 + height2
    && y1 + height1 > y2
    where 
        -- Calculate coords of left bottom corner
        (x1, y1) = (xCentre1 - width1/2, yCentre1 - height1/2)
        (x2, y2) = (xCentre2 - width2/2, yCentre2 - height2/2) 

detectCollision c1 c2 (CircleBox r1) (CircleBox r2)
    = distance c1 c2 < r1 + r2

detectCollision 
    (xRect, yRect)
    (xCircle, yCircle)
    (RectangleBox width height)
    (CircleBox radius) 
    | notColliding = False
    | otherwise    = 
        xDistance <= width/2 
        || yDistance <= height/2
        || cornerDistance < radius^2
    where
        -- "shortcut" for some cases to simplify the calculations
        notColliding = xDistance > (width/2 + radius) || yDistance > (height/2 + radius) 

        xDistance = abs (xCircle - xRect)
        yDistance = abs (yCircle - yRect)

        cornerDistance = (xDistance - width/2)^2 + (yDistance - height/2)^2

detectCollision p1 p2 c@(CircleBox _) r@(RectangleBox _ _)
    = detectCollision p2 p1 r c

addPoints :: Point -> Point -> Point
addPoints (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

distance :: Point -> Point -> Float
distance (x1, y1) (x2, y2) = sqrt $ (x1 - x2)^2 + (y1 - y2)^2