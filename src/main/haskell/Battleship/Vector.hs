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

module Battleship.Vector (VectorT(Vector2D), add, (⊕), sub, (⊖), dot, (⊙), mscalar, (⊗), norm, vmap, vzip_with, vfoldr, angle, o_angle, rotate, o_rotate) where 

data VectorT a = Vector2D (a,a)
  deriving (Show,Read,Eq)

class Vector v where
  vmap :: (a -> b) -> v a -> v b
  vzip_with :: (a -> b -> c) -> v a -> v b -> v c
  vfoldr :: (a -> b -> b) -> b -> v a -> b

  add :: (Num a) => v a -> v a -> v a 
  (⊕) :: (Num a) => v a -> v a -> v a
  (⊕) = add
  add v u = vzip_with (+) v u
  
  sub :: (Num a) => v a -> v a -> v a
  (⊖) :: (Num a) => v a -> v a -> v a
  (⊖) = sub
  sub v u = vzip_with (-) v u

  dot :: (Num a) => v a -> v a -> a
  (⊙) :: (Num a) => v a -> v a -> a
  (⊙) = dot
  dot v u = vfoldr (+) 0 (vzip_with (*) v u)

  mscalar :: (Num a) => a -> v a -> v a
  (⊗)     :: (Num a) => a -> v a -> v a
  (⊗)       = mscalar
  mscalar k = vmap (k*)
  
  unit :: (Floating a) => v a -> v a
  unit v = let mag = norm v
           in vmap (/mag) v

  norm :: (Floating a) => v a -> a
  norm v = sqrt (dot v v)
  
  angle :: (RealFloat a) => v a -> v a -> a

  o_angle :: (RealFloat a) => v a -> a
  o_angle v = angle (vmap (*0) v) v

  o_rotate :: (Floating a) => a -> v a -> v a

  rotate :: (Floating a) => a -> v a -> v a -> v a
  rotate a r v = add (o_rotate a (sub v r)) r
  
instance Vector VectorT where
  vmap f (Vector2D (v0,v1)) = Vector2D $ (f v0, f v1)
  vzip_with f (Vector2D (v0,v1)) (Vector2D (u0,u1)) = Vector2D $ (f v0 u0, f v1 u1)
  vfoldr f z (Vector2D (v0,v1)) = (f v0 (f v1 z))
  angle vi vf = atan2 ydelta xdelta
    where
      (Vector2D (xdelta,ydelta)) = sub vf vi

  o_rotate a (Vector2D (x,y)) = Vector2D (x', y')
    where
      x' = (cos a * x) - (sin a * y)
      y' = (sin a * x) + (cos a * y)
