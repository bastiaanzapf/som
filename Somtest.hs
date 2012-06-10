{-# LANGUAGE FlexibleInstances #-} 

module Somtest (initialize,maptoimage,halfioref) where 

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
    a +. b = seq a seq b zipWith (+) a b
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
triples2 (a:b:c:tl) = [a,b,c]:(triples2 tl)

initialize :: (Ix a) => (a,a) -> IO (Array a [Float])
initialize bounds = do gen<-getStdGen
                       let l = triples2 $ randoms gen :: [[Float]]
                       return $ array bounds $ zip (range bounds) (fromList l)

