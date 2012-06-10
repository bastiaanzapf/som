

module Main (main) where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Layout.VBox
import Data.IORef
import Somtest

import Som.Som
import Som.Gui
import Som.Lrn

import Data.Array.IArray
import Data.Array.Base
import Data.Word
--import System.Random

main = do let w=100
              h=100
          arr <- initialize ((0,0),(w,h)) :: IO (Array (Int,Int) [Float])
          som <- newIORef arr
          s<-readIORef som
          initGUI
          window <- windowNew
          pb <- pixbufNew ColorspaceRgb False 8 w h
          image <- imageNewFromPixbuf pb
          maptoimage s image
          button <- buttonNew
          set button [ buttonLabel := "Hello World" ]
          radius <- newIORef 0.1
          datasets <- readLrnFile "Atom.lrn"
          let learndata = map snd datasets
          onClicked button (do r <- halfioref radius
                               s <- readIORef som
                               
                               s_ <- learnbuttonhandler maptoimage r s learndata image
--                               print s_
                               writeIORef som s_)
          vbox <- vBoxNew True 2
          set vbox [ containerChild := image
                   , containerChild := button ]
          set window [ windowDefaultWidth := 400
                     , windowDefaultHeight := 400
                     , containerChild := vbox ]
          onDestroy window mainQuit
          widgetShowAll window
          mainGUI