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

module Battleship.Environment (water_drag, water_fudge_factor) where

import Battleship.Vector as V

-- * Drag coeficiente (fudge factor)
water_fudge_factor :: (Num a) => a
water_fudge_factor = 100

-- * Calculates the water drag force imposed to a given object.
--
-- The simplified model for this is as follows:
-- $drag = \frac{1}{2} p_f u^2 a_f k$
--
-- Where,
--  $p_f$: Density of water;
--  $a_f$: Frontal area;
--  $k$: Drag coeficiente (fudge factor);
water_drag :: V.VectorT Double -> Double -> V.VectorT Double
water_drag u af = V.mscalar (0.5 * af * k) u2
  where
    u2 = V.vmap (\v -> -1 * (signum v) * (v**2)) u
    k  = water_fudge_factor

-- * Contains all aspects of the simulated world
-- data World = World 
--              {
--                -- * The current position in time.
--                --
--                -- The tick is defined as $t = \frac{1}{25} \texttt{seconds}$.
--                -- tick :: Integer
--              }
--   deriving (Show,Eq)
