module Logic (handleInput, updateWorld) where 

import Graphics.Gloss
import CommonData

data Direction 
    = Left
    | Right

data Action
    = Move Direction
    | Jump

-- Receive events and update the world
-- Example at https://mmhaskell.com/blog/2019/3/25/making-a-glossy-game-part-1
handleInput :: Event -> World -> World
handleInput (EventKey (Char 'a') Down _ _) world = _todo


-- Update entities parameters (position, velocity, acceleration) based on time passed
-- Gravity calculations and collision detection is also here
updateWorld :: Float -> World -> World
update timePassed world = _todo
