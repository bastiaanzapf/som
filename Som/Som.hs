
module Som.Som (DataPoint, Coordinate, (+.), (*.), 
                learn,
                learnpoint, initialize, ddistance, fromList, cdistance) where

import Data.Array.IArray
import Data.Array.ST
import Control.Monad.ST
import System.Random
import Data.Foldable
import Data.Array.Base

class DataPoint a where
    (+.) :: a -> a -> a
    (*.) :: a -> Float -> a
    ddistance :: a -> a -> Float
    fromList :: [Float] -> [a]
                
class Coordinate a where
    cdistance :: a -> a -> Float

class Inf a where
    inf :: a

instance Inf Float where
    inf = read "Infinity"

thawST :: (Ix i, IArray a e) => a i e -> ST s (STArray s i e)
thawST = thaw

minbysnd :: Ord b => (a,b)->(a,b)->(a,b)
minbysnd a b = if (snd a)<(snd b)
                 then a 
                 else b

minarray :: (Ix i, Inf e, Num e, Ord e) => Array i e -> (i,e)
minarray arr = Data.Foldable.foldl minbysnd (head $ range $ bounds arr,inf) (assocs arr)

findclosest :: (Ix i,DataPoint d) => Array i d -> d -> (i,d)
findclosest arr d = let (ix,dist)=minarray (amap (ddistance d) arr)
                    in (ix,arr ! ix)

learndistance :: (DataPoint d,Coordinate c) => Float -> c -> d -> c -> d -> d
learndistance radius ca a cb b = 
    let weight = radius/(radius+(cdistance ca cb))
    in (a *. weight) +. (b *. (1-weight))
                  
mapArrayIx :: (MArray a e' m, MArray a e m, Ix i) => (i -> e' -> e) -> a i e' -> m (a i e)
mapArrayIx f marr = do 
  (l,u) <- getBounds marr
  n <- getNumElements marr
  marr' <- newArray_ (l,u)
  Data.Foldable.sequence_ [do
                              e <- readArray marr i
                              writeArray marr' i (f i e)
                             | i <- range (l,u)]
  return marr'


learnpoint :: (Coordinate i,Ix i,DataPoint d) => Float -> STArray s i d -> d -> ST s (STArray s i d)
learnpoint radius som x = 
    let (y,z) = findclosest som x 
    in mapArrayIx (learndistance radius y z) som

initialize :: (Ix a,DataPoint d) => (a,a) -> IO (Array a d)
initialize bounds = do gen<-getStdGen
                       let l = (randoms gen) :: [Float]
                       return $ array bounds $ zip (range bounds) (fromList l)

learn :: (Ix i,DataPoint d,Coordinate i) => Float -> (Array i d) -> [d] -> Array i d
learn radius som datapoints =
    runST (do som_<-thawST som
              (l,u) <- getBounds som_               
              freeze som_)

test :: (Ix i,IArray a d) => a i d -> ST s ()
test som = do let (l,u) = bounds som 
              som_ <- thawST som
              return ()