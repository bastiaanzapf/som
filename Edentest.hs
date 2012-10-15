{-# LANGUAGE FlexibleInstances #-} 

module Edentest (initialize,halfioref,main) where 

import Som.Parallel
import Som.Lrn
import Som.Som

import Data.Array.IArray
import Data.Array.Base
import Data.IORef
import Data.Word
import System.Random
import Control.Parallel.Eden
import Control.Concurrent

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

initialize :: (Ix a) => (a,a) -> IO (Array a [Float])
initialize bounds = do gen<-getStdGen
                       let l = triples2 $ randoms gen :: [[Float]]
                       return $ array bounds $ zip (range bounds) (fromList l)
triples2 (a:b:c:tl) = [a,b,c]:(triples2 tl)

instance Trans (Array (Int,Int) [Float]) -- genügt schon?

main = do print "Test"
          let w=50
              h=100
          arr1 <- initialize ((0,0),(w,h)) :: IO (Array (Int,Int) [Float])
          arr2 <- initialize ((0,0),(w,h)) :: IO (Array (Int,Int) [Float])
          print "Atest";
          som1 <- newIORef arr1
          som2 <- newIORef arr2
          radius <- newIORef 0.1
          datasets <- readLrnFile "/home/basti/coding/haskell/som/som/Atom.lrn"
          print "Ctest";
          let learndata = map snd datasets
          r  <- halfioref radius
          s1  <- readIORef som1
          s2  <- readIORef som2
          let soms = [s1,s2]
          let u = foldl (learndataparallel r) soms learndata
          print u
          print "Etest";
          