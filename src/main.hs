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

{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Graphics.UI.Gtk                 -- haskell-gtk, gtk3 on Hackage
import Data.Hex               ( hex )  -- haskell-hex, hex on Hackage

import System.Glib.UTFString  ( glibToString )
import Data.Char              ( ord )
import Control.Monad.Reader   ( lift
                              , liftIO
                              , runReaderT
                              , ReaderT
                              , ask
                              )
import Control.Monad          ( when )
import Control.Concurrent.STM ( TVar
                              , readTVarIO
                              , atomically
                              , modifyTVar'
                              , writeTVar
                              , newTVarIO
                              )

data HexOrInt = H | I

data Env = Env { hi   :: TVar HexOrInt
               , rbs  :: (RadioButton, RadioButton)
               , lbl  :: Label
               , win  :: Window
               }

type App = ReaderT Env IO ()

swapHorI :: Env -> IO ()
swapHorI env = readTVarIO (hi env)
  >>= \case
    H -> atomically . writeTVar (hi env) $ I
    I -> atomically . writeTVar (hi env) $ H

buildEnv :: IO Env
buildEnv = 
  builderNew
  >>= \build  -> builderAddFromFile build                 "layout/layout.glade"
  >>             builderGetObject build castToWindow      "win"
  >>= \window -> builderGetObject build castToLabel       "lbl"
  >>= \label  -> builderGetObject build castToRadioButton "rbHex"
  >>= \rbHex  -> builderGetObject build castToRadioButton "rbInt"
  >>= \rbInt  -> newTVarIO (H :: HexOrInt)
  >>= \hori   -> return $ Env hori (rbHex, rbInt) label window

handleEvents :: App 
handleEvents = ask >>= \env -> liftIO $ do
  -- Window
  on (win env) objectDestroy mainQuit
  on (win env) deleteEvent $ liftIO mainQuit >> return False

  -- Radio Button
  on (rbHex env) buttonReleaseEvent $ tryEvent . liftIO $ do
    putStrLn "rbHex clicked"
    toggleButtonSetActive (rbHex env) True
    toggleButtonSetActive (rbInt env) False
    swapHorI env

  -- Radio Button
  on (rbInt env) buttonReleaseEvent $ tryEvent . liftIO $ do
    putStrLn "rbInt clicked"
    toggleButtonSetActive (rbHex env) False
    toggleButtonSetActive (rbInt env) True
    swapHorI env

  -- Keyboard 
  on (win env) keyPressEvent $ do
    kn  <- eventKeyName
    tryEvent . liftIO $ do
      putStrLn $ "key: " ++ glibToString kn
      hori <- readTVarIO (hi env)
      case glibToString kn of
        "Return" -> mainQuit
        str      -> when (length str == 1) $
                      set (lbl env) [ labelText := case hori of
                        H -> str ++ ": " ++ "0x" ++ (hex $ str)
                        I -> str ++ ": " ++ (show . ord . head $ str)
                      ]

  return ()
  where
    rbHex = fst . rbs
    rbInt = snd . rbs

main :: IO ()
main = do
  -- RadioButton State
  hexOrInt <- newIORef H

  initGUI

  -- Build Environment
  env <- buildEnv
  let run = (flip runReaderT) env

  -- Events
  run handleEvents

  widgetShowAll win
  mainGUI
