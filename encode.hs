module Encode 
    where

----------------------------------------------------------------
-- Author:              Ron Watkins
-- Created:             Thu Oct 25 01:43:36 CST 2012
-- Last modified:       Fri Nov  2 15:45:55 CST 2012
-- Program name:        encode.hs 
-- Module name:         Encode
-- Description: 
--  This is the encoding algorhtm for the "Simple Regenerating
--  Codes: Networking Coding for Cloud Storage" Implementation
-- Usage:
--  Run the function "showData" in ghci to view the node 
--  placement of the given input file.
----------------------------------------------------------------


----------------------------------------------------------------
--                  Imports and Dependencies                  --
----------------------------------------------------------------

import Data.List
import Data.Char


----------------------------------------------------------------
--                        Data & Types                        --
----------------------------------------------------------------

-- Erasure code. Defined by n, k, and f
-- n = storage nodes
-- k = n-k erasures
-- f = disks
data Code = Code {n :: Int, k :: Int, f :: Int}

-- Input file type
-- Using an abritrary number for now, but should be a binary 
-- input taken from the command line.
data File = File {m :: Int}

-- Chunk type
-- Chunks are the broken/separated pieces of the original file
data Chunk = Chunk {w :: Int}

-- The matrix type. 
-- Needs to be an abritrarily large number,
-- so might need to migrate it away from [[Int]] and to something
-- like [[Double]] or an arbritrarily large number. Maybe [[a]]. 
-- Need to experiment a bit with it.
type Matrix = [[Int]]

-- Chunk data type.
-- These are the chunks that get arranged into each of the nodes.
-- Same with matrix, this type needs to be changed to an arbritarily
-- large real number. Maybe "Num" or "a"
type Chunk' = Int
type Xn = Chunk'
type Yn = Chunk'
type Sn = Chunk'

-- Node data type.
-- Each node holds 3 chunks. One Xn, one Yn, and one Sn.
type Node = (Xn, Yn, Sn)

----------------------------------------------------------------
--                        Program Variables                   --
----------------------------------------------------------------
-- These are hardcoded but should eventually be supplied by a --
-- config file or via the command line at run time.           --
----------------------------------------------------------------

-- (4,2,2) SRC code definition
src :: Code
src = Code 4 2 2

-- Declare the actual file that will be encoded. This is 
-- currently a number which is hard coded as an Int via the
-- "File" data type. It will need to be changed to be taken as
-- binary data input from the command line.
file :: File
file = File 324533 --Hard coded file for testing

-- MDS generator matrix. See appendix for supplemental matrices
mdsGen :: Matrix
mdsGen = [  [1,0,1,2]   ,
            [0,1,1,1]   ]

----------------------------------------------------------------
--                        File Encoding                       --
----------------------------------------------------------------

-- The file in string format for easy viewing and chunking.
-- This could be a cause of some optimization problems in the 
-- future and needs to have some experimentation done to make
-- sure it is lazy and not loading an entire file into memory at
-- once.
viewFile :: File -> String
viewFile (File m) = show m
file' = viewFile file

-- Split the file into 'n' amount of chunks for processing.
-- This 'n' number is currently hard coded but needs to eventually
-- be taken from the "n" of the "Code" data type
chunk n xs = chunk' i xs
    where
        chunk' _ [] = []
        chunk' n xs = a : chunk' n b where (a,b) = splitAt n xs
        i = ceiling (fromIntegral (length xs) / fromIntegral n)

-- Chunk file' into 'n' pieces then format it for the next step
-- in preparation of sending it to the MDS generator
-- The 'n' will need to come from the "Code" data type later.
chunking = chunk 4 file'
chunked = map read chunking :: [Chunk']

-- [f1,f2] and [f3,f4] chunks for multiplying with the md5 gen.
-- The take2 drop2 method needs to be changed in the future to
-- scale for different "Code" sizes and different amount of 
-- servers.
f1f2 = [take 2 chunked] :: Matrix
f3f4 = [drop 2 chunked] :: Matrix

-- Create the sparse codes: [x1,x2,x3,x4] and [y1,y2,y3,y4]
-- for specifying the four parity chunks later. 
-- This code also rotates the chunks into the correct position
-- for placement in the nodes. This rotation code should be 
-- reviewed and possibly be made into its own function later.
x14 = rotate (concat $ f1f2 `multMatrix` mdsGen) 0 :: [Xn] 
y14 = rotate (concat $ f3f4 `multMatrix` mdsGen) 1 :: [Yn]
-- Create the parity set for XORing later in retrieval
s14 = rotate (zipWith (+) x14 y14) 2 :: [Sn] 

-- Put the individual chunks into separate nodes based on 
-- their correct intended placement on the server.
nodes = zip3 x14 y14 s14 :: [Node]


----------------------------------------------------------------
--                        Data Display                        --
----------------------------------------------------------------

-- Node printing
xNodes = map fstN nodes
showX = untabs $ map show xNodes

yNodes = map sndN nodes
showY = untabs $ map show yNodes

sNodes = map thdN nodes
showS = untabs $ map show sNodes

-- IO Monad for displaying the current data stored in the nodes.
showData = do
    putStrLn "Input file is: "
    putStrLn file'
    putStrLn "Nodes:\t1 \t2 \t 3\t 4"
    putStr "    X\t"
    putStrLn showX
    putStr "    Y\t"
    putStrLn showY
    putStr "    S\t"
    putStrLn showS


----------------------------------------------------------------
--                        Misc Functions                      --
----------------------------------------------------------------

-- Node extraction
fstN :: Node -> Chunk'
fstN (x,_,_) = x

sndN :: Node -> Chunk'
sndN (_,y,_) = y

thdN :: Node -> Chunk'
thdN (_,_,z) = z

-- Rotate function for rotating the chunks inside the nodes.
rotate xs n = take (length xs) $ drop (length xs + n) $ cycle xs

-- View code datatype values. Easy printing.
viewCode :: Code -> String
viewCode (Code n k f) = "n: " ++ show n ++ " k: " ++ show k ++ " f: " ++ show f

-- Matrix multiplication
multMatrix :: Matrix -> Matrix -> Matrix
multMatrix a b = [ [sum $ zipWith (*) ar bc | bc <- transpose b] | ar <- a]

-- Untabs takes a list of strings, then puts a tab between each
-- for easier printing.
untabs :: [String] -> String
untabs = concatMap (++ "\t")


----------------------------------------------------------------
--                        Data IO                             --
----------------------------------------------------------------
-- Nothing here yet.


----------------------------------------------------------------
--                        Appendix                            --
----------------------------------------------------------------

-- Some MDS matrices (from linear.pdf, page 34)
--  Examples of [4,2] sized MDS generator matrices:
--
--          | 1 0 1 2 |
--          | 0 1 1 1 |
--
--              and
--
--          | 0 1 1 1 |
--          | 1 1 2 0 |
--
