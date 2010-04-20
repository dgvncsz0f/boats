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

module Battleship.Movable (Movable, rudder, engine, cmradius, weight, mass, farea, water_drag, acceleration, velocity, space) where

import qualified Control.Monad.State as S
import qualified Battleship.Vector as V
import qualified Battleship.Environment as E

-- * Represents an object (ship) which is able to move.
--
-- Minimum complete definition: (rudder, engine, weight, farea)
class Movable s where
  -- * The angle of the rudder in radians (x | 0 ≤ x ≤ π/2).
  rudder :: s -> Double
  
  -- * The force of the engine of this ship (N).
  -- (F = ma) <=> a = 9.8F/W
  engine :: s -> Double

  -- * The inertial mass of this ship (kg)
  -- (W = mg) <=> m = W/9.8
  weight :: s -> Double

  -- * The front area of this ship (used to calculate the water drag force)
  -- (meters)
  farea :: s -> Double

  -- * Radius from the CM
  cmradius :: s -> Double

  -- * The mass using the gravity of Earth
  mass :: s -> Double
  mass s = weight s / 9.8
  
  -- * Computes the water drag force for a given ship.
  water_drag :: s -> V.VectorT Double -> V.VectorT Double
  water_drag s v0 = E.water_drag v0 (farea s)

  -- * The acceleration of this object.
  acceleration :: s -> V.VectorT Double -> V.VectorT Double
  acceleration s v0 = V.add al' ag'
    where
      ang' = V.o_angle v0
      fd'  = water_drag s v0
      fm'  = V.Vector2D (engine s * cos ang', engine s * sin ang')
      t'   = cmradius s * (V.norm fd') * sin (rudder s)
      mi'  = mass s * (cmradius s) / 2

      ag'  = V.Vector2D (t'/mi' * cos (ang'+pi/2), t'/mi' * sin (ang'+pi/2))
      al'  = V.vmap (/mass s) (V.add fm' fd')

  -- * The velocity of this object in terms of time of an initial velocity v0.
  velocity :: s -> S.State (V.VectorT Double) (V.VectorT Double)
  velocity s = do 
    v0 <- S.get
    S.put $ V.add v0 (acceleration s v0)
    return v0

  -- * Calculate the space in terms of an initial velocity v0 and space v0.
  space :: s -> S.State (V.VectorT Double, V.VectorT Double) (V.VectorT Double, V.VectorT Double)
  space s = do
    (v0, s0) <- S.get
    let s1 = V.add s0 v0
        v1 = S.execState (velocity s) v0
    S.put (v1, s1)
    return (v0, s0)

