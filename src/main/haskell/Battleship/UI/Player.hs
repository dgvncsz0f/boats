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

module Battleship.UI.Player (draw_player, draw_console) where

import qualified Battleship.Battleship as B
import qualified Battleship.Vector as V
import qualified Battleship.Game.Game as G
import qualified Battleship.Movable as M
import qualified Graphics.Rendering.Cairo as C
import qualified Text.Printf as P

_fixp :: Double -> Double
_fixp k | k>1 = k - fromInteger (floor k)
        | k<0 = k - fromInteger (floor k)
        | otherwise = k

_scale :: B.BattleshipT -> Double
-- _scale (B.AircraftCarrier _ _) = 0.15
-- _scale (B.Submarine _ _)       = 0.15
_scale _                       = 0.2

_which_screen :: (Num a) => Double -> a
_which_screen k | k>1       = fromInteger (floor k)
                | k<0       = fromInteger (floor $ recip k)
                | otherwise = 0

_draw_ship :: Double -> Double -> Double -> Double -> Double -> C.Render ()
_draw_ship a mx my sw sh = do
  C.setLineCap C.LineCapSquare
  C.setLineJoin C.LineJoinMiter
   
  C.translate (mx) (my)
  C.rotate a
  C.translate (-mx) (-my)

  C.rectangle (mx-sw) (my-sh) (sw*2) (sh*2)
  C.setSourceRGB 0 0 0
  C.fill

  C.rectangle (mx+sw*0.8) (my-sh) (sw*0.2) (sh*2)
  C.setSourceRGB 1 0 0
  C.fill

draw_player :: G.PlayerT -> G.PlayerT -> Double -> Double -> C.Render ()
draw_player p o w h = do
  let a     = V.o_angle (fst $ G.space0 p)
      s     = G.ship p
      ws    = (_scale s * (B.size s) / w) / 2
      hs    = (_scale s * (B.beam s) / h) / 2
      (V.Vector2D (ofx0,ofy0)) = V.vmap (_scale s*) (snd $ G.space0 p)
      (ofx,ofy)                = (_fixp $ ofx0/w, _fixp $ ofy0/h)
      
      screenx = _which_screen (ofx0/w)
      screeny = _which_screen (ofy0/h)

      o_a     = V.o_angle (fst $ G.space0 o)
      o_s     = G.ship o
      o_ws    = (_scale s * (B.size o_s) / w) / 2
      o_hs    = (_scale s * (B.beam o_s) / w) / 2
      (V.Vector2D (o_ofx,o_ofy)) = V.vmap (_scale s*) (snd $ G.space0 o)

  C.save
  C.scale w h
  _draw_ship a ofx ofy ws hs
  C.restore

  C.save
  C.scale w h
  _draw_ship o_a ((o_ofx-w*screenx) / w) ((o_ofy-h*screeny) / h) o_ws o_hs
  C.restore

draw_console :: G.PlayerT -> C.Render()
draw_console p = do
  let s                      = G.ship p
      v@(V.Vector2D (vx,vy)) = fst $ G.space0 p
      (V.Vector2D (sx,sy))   = snd $ G.space0 p
      (V.Vector2D (ax,ay))   = M.acceleration s v
      (V.Vector2D (dx,dy))   = M.water_drag s v

  C.save
  C.setSourceRGB 1 0 0
  C.moveTo 10 10
  C.showText $ "Rudder: " ++ P.printf "%+.1f (deg)" (M.rudder s * 180 / pi)
  C.moveTo 10 20
  C.showText $ "Engine: " ++ P.printf "%.0f (%%)" (B.engine_power s * 100)
  C.setSourceRGB 0 0 0
  C.moveTo 10 30
  C.showText $ "Velocity: " ++ P.printf "(%+.2f, %+.2f)" vx vy
  C.moveTo 10 40
  C.showText $ "Space: " ++ P.printf "(%+.2f, %+.2f)" sx sy
  C.moveTo 10 50
  C.showText $ "Acceleration: " ++ P.printf "(%+.4f, %+.4f)" ax ay
  C.moveTo 10 60
  C.showText $ "Water drag: " ++ P.printf "(%+.2f, %+.2f)" dx dy
  C.restore
