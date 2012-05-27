
module Som.Som (DataPoint, Coordinate, (+.), (*.), 
                learn,
                learnpoint, initialize, ddistance, fromList, cdistance) where

import Data.Array.IArray
import Data.Array.Base
import Random

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


minbysnd :: Ord b => (a,b)->(a,b)->(a,b)
minbysnd a b = if (snd a)<(snd b)
                 then a 
                 else b

minarray :: (Ix i, Inf e, Num e, Ord e) => Array i e -> (i,e)
minarray arr = foldl minbysnd (head $ range $ bounds arr,inf) (assocs arr)

findclosest :: (Ix i,DataPoint d) => Array i d -> d -> (i,d)
findclosest arr d = let (ix,dist)=minarray (amap (ddistance d) arr)
                    in (ix,arr ! ix)

learndistance :: (DataPoint d,Coordinate c) => Float -> c -> d -> c -> d -> d
learndistance radius ca a cb b = 
    let weight = radius/(radius+(cdistance ca cb))
    in (a *. weight) +. (b *. (1-weight))

amapix :: (IArray a e', IArray a e, Ix i) => (i -> e' -> e) -> a i e' -> a i e
amapix f arr = case bounds arr of
                 (l,u) -> array (l,u) $ zip (range (l,u)) $ map (uncurry f) (assocs arr)


learnpoint :: (Coordinate i,Ix i,DataPoint d) => Float -> Array i d -> d -> Array i d
learnpoint radius som x = 
    let (y,z) = findclosest som x 
    in amapix (learndistance radius y z) som

initialize :: (Ix a,DataPoint d) => (a,a) -> IO (Array a d)
initialize bounds = do gen<-getStdGen
                       let l = (randoms gen) :: [Float]
                       return $ array bounds $ zip (range bounds) (fromList l)

foldlearn :: (Ix i,DataPoint d) => (Array i d -> d -> Array i d) -> Array i d -> [d] -> Array i d
foldlearn l som (p:tp) = foldlearn l (l som p) tp
foldlearn l som [] = som

learn :: (Ix i,DataPoint d,Coordinate i) => Float -> (Array i d) -> [d] -> (Array i d)    
learn radius som datapoints =
    foldlearn (learnpoint radius) som datapoints
