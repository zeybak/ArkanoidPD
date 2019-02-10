module Input where

import Settings
import World
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

{- Suscribe to player's input events -}
handlePlayerInputs :: Event -> WorldObject -> WorldObject
handlePlayerInputs (EventKey (Char 'a') Up _ _) player = player { velocity = setVelocity (0, 0) player } -- Stop player
handlePlayerInputs (EventKey (Char 'd') Up _ _) player = player { velocity = setVelocity (0, 0) player } -- Stop player
handlePlayerInputs (EventKey (Char 'a') Down _ _) player = player { velocity = setVelocity (-playerSpeed generalSettings, 0) player } -- Move player to the left
handlePlayerInputs (EventKey (Char 'd') Down _ _) player = player { velocity = setVelocity (playerSpeed generalSettings, 0) player } -- Move player to the right
handlePlayerInputs _ game = game -- If no event key is registered, don't change anything