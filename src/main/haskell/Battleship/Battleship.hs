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

module Battleship.Battleship (BattleshipT(AircraftCarrier, Battleship, Cruiser, Destroyer, Submarine), engine_power, rudder_angle, size, beam, beam_wl) where

import qualified Battleship.Movable as M
import qualified Battleship.Environment as E

-- * Avaiable Missiles
-- data MissileT  = StandardMissile

-- * The availables ships in this game
-- http://en.wikipedia.org/wiki/USS_Enterprise_(CVN-65)
-- http://en.wikipedia.org/wiki/USS_Iowa_(BB-61)
-- http://en.wikipedia.org/wiki/USS_Port_Royal_(CG-73)
-- http://en.wikipedia.org/wiki/USS_Winston_S._Churchill_(DDG-81)
-- http://en.wikipedia.org/wiki/USS_Nautilus_(SSN-571)
data BattleshipT =  AircraftCarrier { engine_power  :: Double,
                                      rudder_angle :: Double
                                    }
                  | Battleship { engine_power  :: Double,
                                 rudder_angle :: Double
                               }
                  | Cruiser { engine_power  :: Double,
                              rudder_angle :: Double
                            }
                  | Destroyer { engine_power  :: Double,
                                rudder_angle :: Double
                              }
                  | Submarine { engine_power  :: Double,
                                rudder_angle :: Double
                              }
  deriving (Show,Eq)

-- * The length of a given ship
size :: BattleshipT -> Double
size (AircraftCarrier _ _) = 342
size (Battleship _ _)      = 270
size (Cruiser _ _)         = 173
size (Destroyer _ _)       = 155
size (Submarine _ _)       = 97

-- * The beam size (waterline) of a given ship
beam_wl :: BattleshipT -> Double
beam_wl (AircraftCarrier _ _) = 41
beam_wl (Battleship _ _)      = 33
beam_wl (Cruiser _ _)         = 17
beam_wl (Destroyer _ _)       = 20
beam_wl (Submarine _ _)       = 9

-- * The beam size (extremes) of a given ship
beam :: BattleshipT -> Double
beam (AircraftCarrier _ _) = 78
beam (Battleship _ _)      = 33
beam (Cruiser _ _)         = 17
beam (Destroyer _ _)       = 20
beam (Submarine _ _)       = 9

instance M.Movable BattleshipT where
  rudder                         = rudder_angle
  engine s                        = (engine_power s) * 50 * E.water_fudge_factor * (beam_wl s) -- ~10 m/s
  farea                           = beam_wl
  cmradius                        = (/2) . size
  weight (AircraftCarrier _ _)  = 93500 * 1000
  weight (Battleship _ _)       = 45000 * 1000
  weight (Cruiser _ _)          = 9600  * 1000
  weight (Destroyer _ _)        = 9350  * 1000
  weight (Submarine _ _)        = 3520  * 1000
