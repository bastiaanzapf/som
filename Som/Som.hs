
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

minarray :: (Ix i, Inf e, Num e, Ord e,MArray (STArray a) e (ST s)) => STArray a i e -> ST s (i,e)
minarray arr = do as <- getAssocs arr
                  (l,u) <- getBounds arr
                  e <- readArray arr l
                  return $ Data.Foldable.foldl minbysnd 
                             (head $ range $ (l,u),inf) 
                             as


findclosest :: (Ix i,DataPoint d,Num e, Inf d,
                MArray (STArray a) Float (ST s),
                MArray (STArray a) d (ST s),
                MArray (STArray a) e (ST s)) => 
                   STArray a i d -> d -> ST s (i,d) --  {    } ... o.0

{- findclosest arr d = do arr_ <- mapArray (ddistance d) arr
                       (l,u) <- getBounds arr_
                       e <- readArray arr l
                       Data.Foldable.foldlM
                       return (l,e) -}

findclosest arr d = do as<-getAssocs arr
                       (l,u) <- getBounds arr
                       e <- readArray arr l
                       return $ Data.Foldable.foldl minbysnd 
                             (l,inf)
                             as
                       

learndistance :: (DataPoint d,Coordinate c) => Float -> c -> d -> c -> d -> d
learndistance radius ca a cb b = 
    let weight = radius/(radius+(cdistance ca cb))
    in (a *. weight) +. (b *. (1-weight))
                  
mapArrayIx :: (MArray a e m, Ix i) => (i -> e -> e) -> a i e -> m (a i e)
mapArrayIx f marr = do 
  (l,u) <- getBounds marr
  n <- getNumElements marr
  Data.Foldable.sequence_ [do
                              e <- readArray marr i
                              writeArray marr i (f i e)
                             | i <- range (l,u)]
  return marr


learnpoint :: (Coordinate i,Ix i,DataPoint d,Inf d) => Float -> STArray s i d -> d -> ST s (STArray s i d)
learnpoint radius som tolearn = 
    do  (coord,elt) <- findclosest som tolearn
        mapArrayIx (learndistance radius coord elt) som

initialize :: (Ix a,DataPoint d) => (a,a) -> IO (Array a d)
initialize bounds = do gen<-getStdGen
                       let l = (randoms gen) :: [Float]
                       return $ array bounds $ zip (range bounds) (fromList l)

learn :: (Ix i,DataPoint d,Coordinate i,Inf d) => Float -> (Array i d) -> [d] -> Array i d
learn radius som datapoints =
    runST (do som_<-thawST som
              Data.Foldable.foldlM (learnpoint radius) som_ datapoints
              freeze som_)

