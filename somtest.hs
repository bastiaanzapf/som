{-# LANGUAGE FlexibleInstances #-} 

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

instance DataPoint (Float,Float,Float) where
    (a1,a2,a3) +. (b1,b2,b3) = (a1+b1,a2+b2,a3+b3)
    (a1,a2,a3) *. s = (s*a1,s*a2,s*a3)
    ddistance (p1,p2,p3) (q1,q2,q3) = (p1-q1)^2+(p2-q2)^2+(p3-q3)^2
    fromList (a:b:c:tl) = (a,b,c):(fromList tl)
    dzero = (0,0,0)

instance Inf a => Inf (a,a,a) where
    inf = (inf,inf,inf)

instance Coordinate (Int,Int) where
    cdistance (a1,b1) (a2,b2) = sqrt $ fromIntegral ((a1-a2)^2+(b1-b2)^2)


halfioref ioref =
    do a <- readIORef ioref
       writeIORef ioref (a/2)
       return a

colors = [(0.9273889,0.24430194,0.89062494),(0.7507044,0.3714233,0.97739625),(7.7857584e-2,2.2540182e-2,0.24666712),(0.48705852,0.39035508,0.6412518),(0.5506758,0.8146796,0.18742311),(6.7243904e-2,0.9250506,0.99112403),(0.23312452,0.8008623,5.646351e-2),(0.34950817,0.8928416,0.6971415),(0.6078604,0.7119869,0.5969941),(0.49356803,0.94624186,0.9165472)]

maptoimage :: Array (Int,Int) (Float,Float,Float) -> Image -> IO ()
maptoimage i image = 
    do pb <- imageGetPixbuf image
       pbData <- (pixbufGetPixels pb :: IO (PixbufData Int Word8))
       rowstride <- pixbufGetRowstride pb
       w <- pixbufGetWidth pb
       h <- pixbufGetHeight pb
       transferpixel rowstride pbData i 0 0
       sequence_ [transferpixel rowstride pbData i x y | x<-[0..w-1], y<-[0..h-1] ]
       imageSetFromPixbuf image pb

triples (a:b:c:_) = (a,b,c)

main = do let w=100
              h=100
          arr <- initialize ((0,0),(w,h))
          som <- newIORef arr
          s<-readIORef som
          initGUI
          window <- windowNew
          pb <- pixbufNew ColorspaceRgb False 8 w h
          image <- imageNewFromPixbuf pb
          maptoimage s image
          button <- buttonNew
          set button [ buttonLabel := "Hello World" ]
          radius <- newIORef 5.0
          --          datasets <- readLrnFile "Atom.lrn"
          --          let datasets = colors
          --          let learndata = take 3 $ map (triples.snd) datasets
          let learndata = colors
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