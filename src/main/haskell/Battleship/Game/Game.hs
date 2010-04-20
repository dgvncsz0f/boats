{-# LANGUAGE UnicodeSyntax #-}

-- Copyright (C) 2008 Diego Souza <paravinicius@yahoo.com.br>
-- 
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
-- 
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License along
-- with this program; if not, write to the Free Software Foundation, Inc.,
-- 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

module Battleship.Game.Game (PlayerT(HumanPlayer), GameT(TwoPlayerGame), ship, space0, player1, player2, time_step, exec_command) where

import qualified Battleship.Battleship as B
import qualified Battleship.Vector as V
import qualified Battleship.Movable as M
import qualified Control.Monad.State as ST

-- * The possible players of the game
data PlayerT = HumanPlayer
              { -- * The ship this player is going to use to play
                ship   :: B.BattleshipT,
                -- * The initial position (changes on every step, instead of a function in terms of time)
                space0 :: (V.VectorT Double, V.VectorT Double)
              }

-- * The current game. The game contains information about the two players.
data GameT = TwoPlayerGame 
             { player1 :: PlayerT,
               player2 :: PlayerT
             }

-- * The maximum angle which the rudder of each ship can perform.
max_angle :: (Floating a) => a
max_angle = pi/2 

-- * Performs a time step, so calculate the new positions of every single object in the game.
--
-- The time step represents 1 second. The result of this operation is the game
-- state updated, which you should save for further time_steps.
time_step :: GameT -> GameT
time_step (TwoPlayerGame p1 p2) = 
  TwoPlayerGame (time_step_for' p1) (time_step_for' p2)
  where
    time_step_for' p@(HumanPlayer obj s0) = 
      p { space0 = ST.execState (M.space obj) s0 }

-- * Update the engine power factor of a given player's ship.
set_engine_power :: PlayerT -> Double -> PlayerT
set_engine_power p@(HumanPlayer s _) v | v>=(0) && v<=1 = do_change'
                                       | otherwise       = p
  where
    new_ship'  = s { B.engine_power = v }
    do_change' = p { ship = new_ship' }

-- * Extracts the engine power factor of a given player's ship.
engine_power :: PlayerT -> Double
engine_power p = B.engine_power (ship p)

-- * Update the rudder angle of a given player's ship.
set_rudder_angle :: PlayerT -> Double -> PlayerT
set_rudder_angle p@(HumanPlayer s _) v | v>=(-max_angle) && v<=(max_angle) = do_change' v
                                        | v<(-max_angle)                    = do_change' (-max_angle)
                                        | otherwise                         = do_change' max_angle
  where
    new_ship'  v1 = s { B.rudder_angle = v1 }
    do_change' v1 = p { ship = new_ship' v1 }

-- * Extracts the rudder angle of a given player's ship.
rudder_angle :: PlayerT -> Double
rudder_angle p = B.rudder_angle (ship p)

-- * This is used to standardize the keybidings.
--
-- Currently the bindings are defined as follows:
--   w//s :: change engine power by a factor 0f 0.1 (w increases, s decreases)
--   a//d :: change rudder angle by a factor of 0.01 (a increases, d decreases)
exec_command :: Char -> GameT -> GameT
exec_command key g@(TwoPlayerGame p1 p2) | key=='w'  = g { player1 = set_engine_power p1 (engine_power p1 + 0.1) }
                                         | key=='s'  = g { player1 = set_engine_power p1 (engine_power p1 - 0.1) }
                                         | key=='q'  = g { player1 = set_engine_power p1 0 }
                                         | key=='a'  = g { player1 = set_rudder_angle p1 (rudder_angle p1 + (max_angle/10)) }
                                         | key=='d'  = g { player1 = set_rudder_angle p1 (rudder_angle p1 - (max_angle/10)) }
                                         | key=='e'  = g { player1 = set_rudder_angle p1 0 }
                                         | key=='i'  = g { player2 = set_engine_power p2 (engine_power p2 + 0.1) }
                                         | key=='k'  = g { player2 = set_engine_power p2 (engine_power p2 - 0.1) }
                                         | key=='u'  = g { player2 = set_engine_power p2 0 }
                                         | key=='j'  = g { player2 = set_rudder_angle p2 (rudder_angle p2 + (max_angle/10)) }
                                         | key=='l'  = g { player2 = set_rudder_angle p2 (rudder_angle p2 - (max_angle/10)) }
                                         | key=='o'  = g { player2 = set_rudder_angle p2 0 }
                                         | otherwise = g
