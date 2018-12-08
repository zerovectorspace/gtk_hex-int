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

import Text.Printf
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

data BaseConv = H | I | B

type RadioButtons = (RadioButton,RadioButton,RadioButton)

data Env = Env { hib  :: TVar BaseConv
               , rbs  :: RadioButtons
               , lbl  :: Label
               , win  :: Window
               }

type App = ReaderT Env IO ()

setConv :: Env -> BaseConv -> IO ()
setConv env val =
  atomically . writeTVar (hib env) $ val

buildEnv :: IO Env
buildEnv = 
  builderNew
  >>= \build  -> builderAddFromFile build                 "layout/layout.glade"
  >>             builderGetObject build castToWindow      "win"
  >>= \window -> builderGetObject build castToLabel       "lbl"
  >>= \label  -> builderGetObject build castToRadioButton "rbHex"
  >>= \rbHex  -> builderGetObject build castToRadioButton "rbInt"
  >>= \rbInt  -> builderGetObject build castToRadioButton "rbBin"
  >>= \rbBin  -> newTVarIO (H :: BaseConv)
  >>= \hori   -> return $ Env hori (rbHex,rbInt,rbBin) label window

handleEvents :: App 
handleEvents = ask >>= \env@(Env hori (rbHex,rbInt,rbBin) label window ) -> liftIO $ do
  -- Window
  on window objectDestroy mainQuit
  on window deleteEvent $ tryEvent . liftIO $ mainQuit

  -- Radio Button
  on rbHex buttonReleaseEvent $ tryEvent . liftIO $ do
    putStrLn "rbHex clicked"
    toggleButtonSetActive rbHex True
    setConv env H

  -- Radio Button
  on rbInt buttonReleaseEvent $ tryEvent . liftIO $ do
    putStrLn "rbInt clicked"
    toggleButtonSetActive rbInt True
    setConv env I

  -- Radio Button
  on rbBin buttonReleaseEvent $ tryEvent . liftIO $ do
    putStrLn "rbBin clicked"
    toggleButtonSetActive rbBin True
    setConv env B

  -- Keyboard 
  on window keyPressEvent $ do
    kn  <- eventKeyName
    tryEvent . liftIO $ do
      putStrLn $ "key: " ++ glibToString kn
      hib <- readTVarIO (hib env)
      case glibToString kn of
        "Return" -> mainQuit
        str      -> when (length str == 1) $
                      set (lbl env) [ labelText := case hib of
                        H -> printf "%s: 0x%x" str (ord . head $ str) :: String
                        I -> printf "%s: %i"   str (ord . head $ str) :: String
                        B -> printf "%s: 0b%b" str (ord . head $ str) :: String
                      ]

  return ()

main :: IO ()
main =
  -- Intialize Gtk
  initGUI

  -- Build GUI & Environment
  >> buildEnv >>= \env ->
  let run = (flip runReaderT) env in

  -- Events
  run handleEvents

  -- Show Window
  >> widgetShowAll (win env)

  -- Main Event Loop
  >> mainGUI
