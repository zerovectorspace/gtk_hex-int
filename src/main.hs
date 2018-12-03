{-
  This file is part of gtk_hex-int.

  fct is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  fct is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with fct.  If not, see <http://www.gnu.org/licenses/>.

  Copyright 2018 Zachary Young
  -}

module Main (main) where

import Graphics.UI.Gtk -- haskell-gtk, gtk3 on Hackage
import Data.Hex        -- haskell-hex, hex on Hackage

import System.Glib.UTFString
import Data.IORef
import Data.Char
import Control.Monad.Trans

data HexOrInt = H | I

keyPressed :: (LabelClass a) => a -> IORef HexOrInt -> EventM EKey Bool
keyPressed lbl ref = tryEvent $ do
  kn <- eventKeyName
  hori <- liftIO $ readIORef ref
  liftIO $ case glibToString kn of
    "Return" -> mainQuit
    str -> set lbl [ labelText := case hori of
                     H -> str ++ ": " ++ "0x" ++ (hex $ str)
                     I -> str ++ ": " ++ (show . ord . head $ str)
                   ]

radHexPressed :: (ToggleButtonClass a, ToggleButtonClass b)
              => a -> b -> IORef HexOrInt -> EventM c Bool
radHexPressed radHex radInt ref = tryEvent . liftIO $ do
  hori <- readIORef ref
  toggleButtonSetActive radHex True
  toggleButtonSetActive radInt False
  case hori of
    H -> writeIORef ref I
    I -> writeIORef ref H

radIntPressed :: (ToggleButtonClass a, ToggleButtonClass b)
              => a -> b -> IORef HexOrInt -> EventM c Bool
radIntPressed radHex radInt ref = tryEvent . liftIO $ do
  hori <- readIORef ref
  toggleButtonSetActive radHex False
  toggleButtonSetActive radInt True
  case hori of
    H -> writeIORef ref I
    I -> writeIORef ref H

main :: IO ()
main = do
  -- RadioButton State
  hexOrInt <- newIORef H

  initGUI

  -- Glade layout
  build <- builderNew
  builderAddFromFile build "layout/layout.glade"

  -- Windows
  win <- builderGetObject build castToWindow "win"

  -- Label
  lbl <- builderGetObject build castToLabel "lbl"

  -- Radio Buttons
  radHex <- builderGetObject build castToRadioButton "radHex"
  radInt <- builderGetObject build castToRadioButton "radInt"

  -- Events
  on win objectDestroy mainQuit
  on win deleteEvent $ liftIO mainQuit >> return False

  -- Events: Radio Buttons
  on radHex buttonReleaseEvent (radHexPressed radHex radInt hexOrInt)
  on radInt buttonReleaseEvent (radIntPressed radHex radInt hexOrInt)

  -- Events: Keyboard 
  on win keyPressEvent (keyPressed lbl hexOrInt)

  widgetShowAll win
  mainGUI
