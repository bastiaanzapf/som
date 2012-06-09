
module Som.Gui (learnbuttonhandler,transferpixel) where 
    
import Som.Som

import Data.Array.Base
import Data.Array.IArray
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Layout.VBox
import Graphics.UI.Gtk.Gdk.Pixbuf
import Data.IORef
import Data.Word


-- Auf einen Knopfdruck reagieren

learnbuttonhandler :: (Inf d,DataPoint d,Coordinate i,Ix i) => (Array i d -> Image -> IO a) -> Float -> Array i d -> [d] -> Image -> IO (Array i d)
learnbuttonhandler maptoimage radius som learnpoints image = 
    do let som_ = learn radius som learnpoints
       maptoimage som_ image
       return som_

halfioref ioref =
    do a <- readIORef ioref
       writeIORef ioref (a/2)
       return a

transferpixel rowstride pbData s x y =
    do let (a,b,c) = s ! (x,y)
       writeArray pbData (x*3+y*rowstride  ) (round (a*255))
       writeArray pbData (x*3+y*rowstride+1) (round (b*255))
       writeArray pbData (x*3+y*rowstride+2) (round (c*255))
