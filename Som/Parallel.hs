
{-# LANGUAGE FlexibleContexts #-} 
{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE MultiParamTypeClasses #-}

module Som.Parallel (learndataparallel) where

import Som.Som 
import Control.Parallel.Eden

import Data.Array.IArray
import Data.STRef
import Data.Array.ST
import Control.Monad.ST

closest :: (Ix i,DataPoint d) => [(i,d)] -> d -> (i,d)

closest (a:[]) _ = a
closest ((i1,d1):tl) d = let (i2,d2) = closest tl d in
                         if ddistance d2 d < ddistance d1 d then
                             (i2,d2) else
                             (i1,d1)

learnsingle radius som datapoint
    = runST (do som_<-thawST som
                learnpoint radius som_ datapoint
                freeze som_)

innerwrapfindclosest :: (Inf d,DataPoint d,Ix i) => (Array i d) -> d -> ST s (i,d)
innerwrapfindclosest som datapoint = do som_ <- thawST som 
                                        findclosest som_ datapoint

wrapfindclosest :: (Inf d,
                    DataPoint d,
                    Ix i
                   ) => (Array i d) -> d -> (i,d)
wrapfindclosest som datapoint = runST (innerwrapfindclosest som datapoint)

innerwraplearn :: (DataPoint d,Ix i,Show d,Coordinate i) => (Array i d) -> Float -> (i,d) -> ST s (Array i d)
innerwraplearn som radius (coord,elt) = do som_ <- thawST som
                                           mapArrayIx (learnDistance radius coord elt) som_
                                           freeze som_
                                           
wraplearn som radius (coord,elt) = runST $ innerwraplearn som radius (coord,elt)

learndataparallel :: (Show d,Show i, Ix i,DataPoint d,Coordinate i,Inf d,Trans (Array i d),Trans i,Trans d) => Float -> [(Array i d)] -> d -> [(Array i d)]
learndataparallel radius som datapoint = 
  let cc=spawn (repeat $ process $ \s-> wrapfindclosest s datapoint) som
      (i,d)=closest cc datapoint
      in spawn (repeat $ process $ \s -> wraplearn s 0.5 (i,d)) som

