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

module Battleship.UI.Window (mainloop) where 

import qualified Battleship.Battleship as B
import qualified Battleship.Vector as V
import qualified Battleship.Game.Game as GM
import qualified Battleship.UI.Player as P
import qualified Data.IORef as I
import qualified Graphics.Rendering.Cairo as C
import qualified Graphics.UI.Gtk as G
import qualified Graphics.UI.Gtk.Glade as GG
import qualified Graphics.UI.Gtk.Gdk.Events as E
import qualified Control.Concurrent as CC
import qualified Control.Monad as M

_draw_game :: (G.WidgetClass widget) => I.IORef GM.GameT -> (GM.GameT -> GM.PlayerT) -> (GM.GameT -> GM.PlayerT) -> widget -> IO Bool
_draw_game rgame player oponent window = do
  (width, height) <- G.widgetGetSize window
  drawwin         <- G.widgetGetDrawWindow window
  game            <- I.readIORef rgame
  
  I.writeIORef rgame (GM.time_step game)

  G.renderWithDrawable drawwin $ do
    C.setSourceRGB 1 1 1
    C.paint
 
    P.draw_player (player game) (oponent game) (fromIntegral width) (fromIntegral height)
    P.draw_console (player game) 

  return True

_draw_radar :: (G.WidgetClass widget) => I.IORef GM.GameT -> widget -> IO Bool
_draw_radar rgame window = do
  (width, height) <- G.widgetGetSize window
  drawwin         <- G.widgetGetDrawWindow window
  game            <- I.readIORef rgame
  
  I.writeIORef rgame (GM.time_step game)

  G.renderWithDrawable drawwin $ do
    C.setSourceRGB 1 1 1
    C.paint
    
    C.save
    
    let w                          = fromIntegral width
        h                          = fromIntegral height
        s0@(V.Vector2D (s0x, s0y)) = snd $ GM.space0 (GM.player1 game)
        s1@(V.Vector2D (s1x, s1y)) = snd $ GM.space0 (GM.player2 game)
        (V.Vector2D (smx, smy))    = V.vzip_with max (V.vmap abs s0) (V.vmap abs s1)
        xpos x                     = x/(2*smx)
        ypos y                     = y/(2*smy)
    
    C.scale w h
    C.setSourceRGB 1 0 0
    C.arc (min 0.9 $ xpos s0x+0.6) (min 0.9 $ ypos s0y+0.6) 0.05 0 (2*pi)
    C.fill
    C.setSourceRGB 0 1 0
    C.arc (min 0.9 $ xpos s1x+0.6) (min 0.9 $ ypos s1y+0.6) 0.05 0 (2*pi)
    C.fill

    C.restore

  return True

_read_ships :: (G.ComboBoxClass a) => a -> IO (B.BattleshipT, B.BattleshipT)
_read_ships combo = do
  ship <- G.comboBoxGetActive combo
  case ship of
    0 -> return (B.AircraftCarrier 0 0, B.AircraftCarrier 0 0)
    1 -> return (B.Battleship 0 0, B.Battleship 0 0)
    2 -> return (B.Destroyer 0 0, B.Destroyer 0 0)
    3 -> return (B.Submarine 0 0, B.Submarine 0 0)
    4 -> return (B.Cruiser 0 0, B.Cruiser 0 0)
    _ -> error "Bye!"

_startup_dialog :: IO (B.BattleshipT, B.BattleshipT)
_startup_dialog = do
  dialogXmlM <- GG.xmlNew "resources/glade/startup.glade"
  let dialogXml = case dialogXmlM 
                  of (Just xml) -> xml
                     Nothing    -> error "can't find the glade file \"startup.glade\" in the current directory"
  dialog   <- GG.xmlGetWidget dialogXml G.castToDialog "dialog1"
  response <- G.dialogRun dialog
  combo    <- GG.xmlGetWidget dialogXml G.castToComboBox "cb_ships"

  case response of
    (G.ResponseUser 1) -> G.widgetHideAll dialog >> _read_ships combo
    _                  -> error $ "Bye!"

mainloop :: IO ()
mainloop = do 
  G.initGUI

  window_p0  <- G.windowNew
  window_p1  <- G.windowNew
  window_rd  <- G.windowNew
  (shipa, shipb) <- _startup_dialog

  G.windowSetTitle window_p0 "Haskell Animated Battleship - Player1"
  G.windowSetTitle window_p1 "Haskell Animated Battleship - Player2"
  G.windowSetTitle window_rd "Haskell Animated Battleship - Radar"
  
  M.forM [(window_p0,500,500), (window_p1,500,500), (window_rd,100,100)] $ \(w, ww, wh) -> do
    G.windowSetDecorated w True
    G.windowSetResizable w True
    -- G.windowSetPosition window_p0 G.WinPosCenterAlways
    G.widgetSetAppPaintable w True
    G.windowSetDefaultSize w ww wh
  
  -- TODO: initialize the game
  let player1 = GM.HumanPlayer shipa (V.Vector2D (0,0), V.Vector2D (1000,500))
      player2 = GM.HumanPlayer shipb (V.Vector2D (0,0), V.Vector2D (1000,2000))

  rgame <- I.newIORef $ GM.TwoPlayerGame player1 player2
  
  M.forM [window_p0, window_p1, window_rd] $ \w -> 
    G.onKeyPress w $ \x ->
      case (E.eventKeyChar x)
      of (Just key) -> I.modifyIORef rgame (GM.exec_command key) >> (return $ E.eventSent x)
         Nothing    -> (return $ E.eventSent x)

  G.timeoutAdd (CC.yield >> _draw_game rgame (GM.player1) (GM.player2) window_p0) 150
  G.timeoutAdd (CC.yield >> _draw_game rgame (GM.player2) (GM.player1) window_p1) 150
  G.timeoutAdd (CC.yield >> _draw_radar rgame window_rd) 150
  
  M.mapM (G.onDestroy >> G.widgetShowAll) [window_p0, window_p1, window_rd]
  G.mainGUI
