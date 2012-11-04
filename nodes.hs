module Main where

-----------------------------------------------------------
-- Author:          Ron Watkins
-- Date Created:    Sun Nov  4 11:52:00 CST 2012
-- File name:       nodes.hs
-- Description:
--  This takes an arbitrary length integer as input, then
--  splits it into 4 nodes and outputs each node as a 
--  binary file into the current directory.
-- To Do:
--  Take binary data as input.
-----------------------------------------------------------

import qualified Data.ByteString as B
import Data.List
import Data.Binary

type Node = (Xn, Yn, Sn)
type Xn = Chunk
type Yn = Chunk
type Sn = Chunk
type Chunk = Integer
type Matrix = [[Integer]]

-- Variables
mdsGen :: Matrix
mdsGen =    [ [1,0,1,2] ,
              [0,1,1,1] ]

-- Node generator
nodes :: Show a => a -> [Node]
nodes n = zip3 x14 y14 s14 :: [Node]
  where
    x14 = rotate (concat $! f1f2 `multMatrix` mdsGen) 0 :: [Xn]
    y14 = rotate (concat $! f3f4 `multMatrix` mdsGen) 1 :: [Yn]
    s14 = rotate (zipWith (+) x14 y14) 2 :: [Sn]
    f1f2 = [take 2 chunked] :: Matrix
    f3f4 = [drop 2 chunked] :: Matrix
    chunked = map read chunking :: [Chunk]
    chunking = chunk 4 (show n)

-- Prepare the file with node numbers for exporting
fileEncode n = zip [1,2,3,4] $ nodes n

-- Functions
rotate :: [a] -> Int -> [a]
rotate xs n = take (length xs) $! drop (length xs + n) $! cycle xs

multMatrix :: Matrix -> Matrix -> Matrix
multMatrix a b = [[sum $! zipWith (*) ar bc | bc <- transpose b] | ar <- a]

chunk :: Integral a => a -> [a1] -> [[a1]]
chunk n xs = chunk' i xs
  where
    chunk' _ [] = []
    chunk' n xs = a : chunk' n b where (a,b) = splitAt n xs
    i = ceiling (fromIntegral (length xs) / fromIntegral n)

fac :: (Enum a, Num a) => a -> a
fac n = product [1..n]

-- Experiment time.
--
--Without bytestring: 0.155s
--main = do
    --putStrLn $ show $ nodes $ fac 10000 
    --
    

    ------- This one works
    --encodeFile "meow.txt" $! nodes $! product [1..10000]
main = do
    mapM (loop') (fileEncode 123123123123123123123)
    putStrLn "Done!"

-- successfully encode and decode the nodes to a file and back
encodeThis :: Show a => a -> IO ()
encodeThis = (\x -> encodeFile "meow.txt" $! nodes x)

decodeThis = decodeFile "meow.txt" :: IO [Node]

loop (x:xs) = do
    putStrLn $ show x
    if xs == [] then putStrLn "Loop Done" else loop (xs)

--loop' :: (Integer,Node) -> Node
loop' (a,b) = do
    encodeFile (show a) (show b)
    putStrLn $ show a
    putStrLn $ show b

