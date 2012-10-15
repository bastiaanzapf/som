{-# LANGUAGE FlexibleContexts #-}

module Som.Som (DataPoint, Coordinate, Inf, (+.), (*.), inf,
                learn, dzero,
                learnpoint, ddistance, fromList, cdistance,
                mapArrayIx, findclosest, learnDistance, thawST) where

import Data.Array.IArray
import Data.STRef
import Data.Array.ST
import Control.Monad.ST
import Data.Foldable
import Data.Array.Base
import System.IO.Unsafe

class DataPoint a where
    (+.) :: a -> a -> a
    (*.) :: a -> Float -> a
    dzero :: a
    ddistance :: a -> a -> Float
    fromList :: [[Float]] -> [a]
    dnorm :: a -> Float
    dnorm x = ddistance dzero x

                
class Coordinate a where
    cdistance :: a -> a -> Float

class Inf a where
    inf :: a

instance Inf Float where
    inf = read "Infinity"

thawST :: (Ix i, IArray a e) => a i e -> ST s (STArray s i e)
thawST = thaw

-- Find best match

findclosest :: (Ix i,DataPoint d,Inf d,
               MArray (STArray a) d (ST s),
               MArray (STArray a) Float (ST s)
               ) => STArray a i d -> d -> ST s (i,d) --  {    } ... o.0

findclosest arr d = do (l,u) <- getBounds arr
                       e <- readArray arr l
                       st <- newSTRef (l,(ddistance d e)::Float)
                       Data.Foldable.sequence_ $
                                 [do (e,dist)<-readSTRef st
                                     f<-readArray arr i 
                                     let dist'=(dnorm (f+.(d*.(-1))))
                                     if (dist' < dist)
                                       then writeSTRef st $ (i,dist')
                                       else return ()
                                  | i<-range (l,u)]
                       (i,d)<-readSTRef st
                       x<-readArray arr i
                       return (i,x)

-- Distance Function
                       
learnDistance :: (Show d,DataPoint d,Coordinate c,Monad m) => Float -> c -> d -> c -> d -> m d
learnDistance radius ca a cb b = return $!
     let weight = {-# SCC weight #-} radius/(radius+(cdistance ca cb)) 
     in {-# SCC weighting #-} seq weight (a *. weight) +. (b *. (1-weight)) 

-- map with indexes

mapArrayIx :: (MArray a e m, Ix i) => (i -> e -> m e) -> a i e -> m (a i e)
mapArrayIx f marr = 
  do (l,u) <- {-# SCC bounds #-} getBounds marr
     Data.Foldable.sequence_ [do e <- {-# SCC read #-} readArray marr i
                                 e' <-{-# SCC write #-} f i e
                                 {-# SCC write #-} writeArray marr i e'
                              | i <- {-# SCC range #-} range (l,u)]
     return marr

-- learn a single point

learnpoint :: (Show d,Show i,Coordinate i,Ix i,DataPoint d,Inf d) => Float -> STArray s i d -> d -> ST s (STArray s i d)
learnpoint radius som tolearn = 
    do  (coord,elt) <- {-# SCC findclosest #-} findclosest som tolearn 
        {-# SCC map #-} mapArrayIx (learnDistance radius coord elt) som 


-- learn a list of points

learn :: (Show d,Show i,Ix i,DataPoint d,Coordinate i,Inf d) => Float -> (Array i d) -> [d] -> (Array i d)
learn radius som datapoints =
    runST (do som_<-thawST som
              Prelude.sequence_ $ map (learnpoint radius som_) datapoints
              freeze som_)

