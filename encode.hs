module Main
    where

-----------------------------------------------------------
-- Author:            Ron Watkins
-- Last modified:     Thu Oct 25 01:43:36 CST 2012
-- Program name:      sim.hs
-- Description: 
--  This program is the basic algorithm given in "Simple
--  Regenerating Codes: Network Coding for Cloud Storage"
--  implemented in haskell.
-- Usage:
--  Run "main" in ghci to see the data in the 4 different
--  nodes for the file given as input.
-----------------------------------------------------------

import Data.List
import Data.Char

-------------------------------------
--          Data  & Types          --
-------------------------------------

-- Code is defined by n, k, and f
-- n = storage nodes
-- k = n-k erasures
-- f = disks
data Code = Code {n :: Int, k :: Int, f :: Int}

-- File
-- This is the data type for the file we will be
-- coding and placing in the nodes
data File = File {m :: Int}

-------------------------------------
--          Data  & Types          --
-------------------------------------

-- Matrix type
type Matrix = [[Int]]

-- The types for the numbers (chunks inside the nodes)
type Xn = Int
type Yn = Int
type Sn = Int

-- Create the type for the "Node" to hold
-- each of the X's Y's and S's for data storage
type Node = (Xn, Yn, Sn)

-------------------------------------
--          Data Encoding          --
-------------------------------------

-- Declare the actual file to encode
myFile :: File
myFile = File 358473

-- The file in string format for chunking
myFile' = viewFile myFile

-- Output the data in the file
viewFile :: File -> String
viewFile (File m) = show m

-- Make the data type for the chunks. Chunks
-- are the pieces that have been broken up from
-- the original file
data Chunk = Chunk {w :: Int}

-- Split a list into 'n' chunks
-- for splitting myFile into separate chunks for processing
chunk n xs = chunk' i xs
    where
        chunk' _ [] = []
        chunk' n xs = a : chunk' n b where (a,b) = splitAt n xs
        i = ceiling (fromIntegral (length xs) / fromIntegral n)

-- Chunk myFile' then format it for the next step of
-- sending it to the MDS generator
chunking = chunk 4 myFile'
chunked = map read chunking :: [Int]

-- Create the [f1,f2] and [f3,f4] chunks for convenient multiplying
-- with the MDS generator. The take2 drop2 method needs to be changed
-- in the future for better scaling. Its not good to hardcode numbers
f1f2 = [take 2 chunked] :: Matrix
f3f4 = [drop 2 chunked] :: Matrix

-- Create the sparse codes: [x1,x2,x3,x4] and [y1,y2,y3,y4] for 
-- specifying the four parity chunks later
-- This code also rotates the links into the right position for the node
x14 = rotate (concat $ f1f2 `multMatrix` mdsGen) 0 :: [Xn]
y14 = rotate (concat $ f3f4 `multMatrix` mdsGen) 1 :: [Yn]
-- Create the parity set
s14 = rotate (zipWith (+) x14 y14) 2 :: [Sn]

-- Put the chunks into their nodes.
nodes = zip3 x14 y14 s14 :: [Node]

-- Rotate our codes for preparation to be placed into the individual 
-- nodes.
rotate xs n = take (length xs) $ drop (length xs + n) $ cycle xs

-- (4,2,2) SRC code definition
src :: Code
src = Code 4 2 2

-- View Code datatype values i.e.: "viewCode src"
viewCode :: Code -> String
viewCode (Code n k f) = "n: " ++ show n ++ " k: " ++ show k ++ " f: " ++ show f

-- MDS generator matrices (from linear.pdf, page 34)
--  Examples of [4,2] sized generator matrices:
--      
--      | 1 0 1 2 |
--      | 0 1 1 1 |
--
--          and
--
--      | 0 1 1 1 |
--      | 1 1 2 0 |
--

-- MDS Generator matrix
mdsGen :: Matrix 
mdsGen = [  [1,0,1,2]   , 
            [0,1,1,1]   ]

-- Matrix Multiplication for MDS generator multiplication
multMatrix :: Matrix -> Matrix -> Matrix
-- multMatrix m1 m2 = [sum $ zipWith (*) x y | x <- m1, y <- transpose m2]
multMatrix a b = [ [sum $ zipWith (*) ar bc | bc <- transpose b] | ar <- a]

-------------------------------------
--          Data Retrieval         --
-------------------------------------

-- Node extraction
fstN :: Node -> Int
fstN (x,_,_) = x

sndN :: Node -> Int
sndN (_,x,_) = x

thdN :: Node -> Int
thdN (_,_,x) = x

-- Node printing
xNodes = map fstN nodes
showX  = untabs $ map show xNodes

yNodes = map sndN nodes
showY  = untabs $ map show yNodes

sNodes = map thdN nodes
showS  = untabs $ map show sNodes

showData = do
        putStrLn "Input file is: "
        putStrLn myFile'
        putStrLn "Nodes:\t1 \t2  \t3  \t4"
        putStr "    X\t"
        putStrLn showX
        putStr "    Y\t"
        putStrLn showY
        putStr "    S\t"
        putStrLn showS

-- Untabs takes a list of strings, then puts a tab
-- between each of them for easy printing.
untabs :: [String] -> String
untabs = concatMap (++ "\t")

-------------------------------------
--          Data IO                --
-------------------------------------

-- The main Function. It will output information about the
-- algorithm.
main = do
    showData
