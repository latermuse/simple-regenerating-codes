module Main where

-----------------------------------------------------------
-- Author:          Ron Watkins
-- Date Created:    Sun Nov  4 11:52:00 CST 2012
-- File name:       nodes.hs
-- Description:
--  This takes an arbitrary length integer as input, then
--  splits it into 4 nodes and outputs each node as a 
--  binary file into the current directory.
-- Usage:
--  Just run 'main' in ghci or compile with basic tags.
--      example: ghc -o nodes nodes.hs
-- To Do:
--  Take binary data as input.
-----------------------------------------------------------

-- performance: .004 -> .007
-- performance: .004 -> .005

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

main = do
    mapM_ loop' (fileEncode 123123123123123123123)
    putStrLn "Done!"

-- successfully encode and decode the nodes to a file and back
encodeThis :: Show a => a -> IO ()
encodeThis x = encodeFile "meow.txt" $! nodes x

decodeThis = decodeFile "meow.txt" :: IO [Node]

-- Just a loop function im playing with
loop (x:xs) = do
    print x
    if xs == [] then putStrLn "Loop Done" else loop xs

--loop' :: (Integer,Node) -> Node
loop' (a,b) = do
    encodeFile (show a) (show b)
    print a
    print b

-- Working with binarystrings

-- binary file length
--bfLength = B.readFile "1" >>= (\x -> (return . B.length) x) 
{-bfLength = do
    x <- B.readFile "1"
    let y = B.pack $ ((B.length x) `div` 4) 
    return (chunk y x)
      where
        chunk n xs = B.split n xs 
-}


chunkBytes :: Int -> B.ByteString -> [B.ByteString]
chunkBytes n xs = chunk' i xs
  where
    chunk' n xs
        | B.null xs = []
        | otherwise = a : chunk' n b where (a,b) = B.splitAt n xs
    i = ceiling (fromIntegral (B.length xs) / fromIntegral n)
