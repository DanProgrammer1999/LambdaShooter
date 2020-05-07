module Logic (handleInput, updateWorld) where 

import Graphics.Gloss
import CommonData

--| Receive events and update the world
--| Need to calculate collisions to not walk through walls
--| Example at https://mmhaskell.com/blog/2019/3/25/making-a-glossy-game-part-1
handleInput :: Event -> World -> World


--| Update entities parameters (position, velocity, acceleration) based on time passed
--| Gravity calculations and collision detection is also here
updateWorld :: Float -> World -> World
