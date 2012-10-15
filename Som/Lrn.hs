
-- .lrn parser

module Som.Lrn (DataSet,readLrnFile) where

import System.IO
import Data.List.Split
import Data.Maybe

type DataSet = (Int,[Float])

nonBlanks = split (dropBlanks $ dropDelims $ oneOf "\t ")

readLrnLine :: Handle -> IO (Maybe DataSet)
readLrnLine handle = do l <- hGetLine handle                        
                        let nums=nonBlanks l
                        if (head l == '%')
                        then return Nothing
                        else return (Just ((read $ head $ nums),
                                           (map read $ tail nums)))
                        
readLrnLines :: Handle -> IO [DataSet]
readLrnLines handle = do eof <- hIsEOF handle
                         if eof 
                         then return []
                         else do d <- (readLrnLine handle)
                                 ds <- (readLrnLines handle)
                                 case d of 
                                   Nothing -> return ds
                                   Just d_ -> return (d_:ds)

                                                    
readLrnFile :: String -> IO [DataSet]
readLrnFile path = do handle <- openFile path ReadMode
                      readLrnLines handle
                 