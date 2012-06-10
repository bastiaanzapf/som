{-# LANGUAGE FlexibleInstances #-} 

module Somtest (initialize) where 

import Som.Som
import Som.Gui
import Som.Lrn

import Data.Array.IArray
import Data.Array.Base
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Layout.VBox
import Graphics.UI.Gtk.Gdk.Pixbuf
import Data.IORef
import Data.Word
import System.Random

instance DataPoint [Float] where
    a +. b = zipWith (+) a b
    a *. b = map (* b) a
    ddistance a b = sum $ map (**2) $ zipWith (-) a b 
    fromList = id
    dzero = repeat 0

instance Inf [Float] where
    inf = repeat inf

instance Coordinate (Int,Int) where
    cdistance (a1,b1) (a2,b2) = sqrt $ fromIntegral ((a1-a2)^2+(b1-b2)^2)


halfioref ioref =
    do a <- readIORef ioref
       writeIORef ioref (a/2)
       return a

colors = [(0.9273889,0.24430194,0.89062494),(0.7507044,0.3714233,0.97739625),(7.7857584e-2,2.2540182e-2,0.24666712),(0.48705852,0.39035508,0.6412518),(0.5506758,0.8146796,0.18742311),(6.7243904e-2,0.9250506,0.99112403),(0.23312452,0.8008623,5.646351e-2),(0.34950817,0.8928416,0.6971415),(0.6078604,0.7119869,0.5969941),(0.49356803,0.94624186,0.9165472)]

maptoimage :: Array (Int,Int) [Float] -> Image -> IO ()
maptoimage i image = 
    do pb <- imageGetPixbuf image
       pbData <- (pixbufGetPixels pb :: IO (PixbufData Int Word8))
       rowstride <- pixbufGetRowstride pb
       w <- pixbufGetWidth pb
       h <- pixbufGetHeight pb
--       transferpixel rowstride pbData i 0 0
       sequence_ [transferpixel rowstride pbData i x y | x<-[0..w-1], y<-[0..h-1] ]
       imageSetFromPixbuf image pb

triples (a:b:c:tl) = [a*0.03+0.5,b*0.03+0.5,c*0.03+0.5]:(triples tl)
triples2 (a:b:c:tl) = [a,b,c]:(triples tl)

initialize :: (Ix a,DataPoint d) => (a,a) -> IO (Array a d)
initialize bounds = do gen<-getStdGen
                       let l = triples2 $ randoms gen :: [[Float]]
                       return $ array bounds $ zip (range bounds) (fromList l)

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
          radius <- newIORef 0.3
          datasets <- readLrnFile "Atom.lrn"
          --          let datasets = colors
          let learndata = take 200 $ map snd datasets
          --          let learndata = colors
          print "and go.."
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