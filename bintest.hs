module Main where

import qualified Data.ByteString.Lazy as BL
import System.Directory
import Data.Int

-- First read the files in the /bintest/out/ folder, then cut it into four pieces and put them in the /bintest/out folder.

inDirectory = "/Users/ron/Code/bintest/in/"
outDirectory = "/Users/ron/Code/bintest/out/" 

-- Variables -----
-- Size of chunk to work on
chunkSize = 30000 :: Int64

-- Zero-indexed; [0,1] = 2 parts
fileParts = 3
-------------------

splitFile x = do
    file <- BL.readFile $ inDirectory ++ x 
    BL.writeFile (outDirectory ++ x) file
    putStrLn $ "Wrote " ++ inDirectory ++ x ++ " to " ++ outDirectory ++ x

--splitter :: Integer -> FilePath -> IO ()
splitter x = do
    fileIn <- BL.readFile $ inDirectory ++ x
    split' 0 x fileIn

main = splitter "moon.avi" >> unsplitter "moon.avi"

modFile x = BL.map (+ 2) x
unModFile x = BL.map (subtract 2) x

split' :: Int -> FilePath -> BL.ByteString -> IO ()
split' n x f = do
    let fileOut = outDirectory ++ x
    let counter = counterNumber n
    BL.appendFile (fileOut ++ "." ++ show counter) (modFile (BL.take chunkSize f))
    if (BL.null f) == True
        then putStrLn "Splitting Done"
        else if (counter + 1) > fileParts
            then split' 0 x (BL.drop chunkSize f)
            else split' (counter + 1)  x (BL.drop chunkSize f)

counterNumber n = cycle [0..fileParts] !! n

-- Open files and send them to get unsplit
unsplitter x = do
    let fileNames = map ((outDirectory ++ ) . (x ++) . ("." ++)) $ map (show) [0..fileParts]
    openedFiles <- mapM BL.readFile fileNames
    unsplit' x openedFiles

-- It needs to get the first N chunks of F file for each FS files. 
unsplit' x fs = do
    let fileOut = inDirectory ++ "new-" ++ x
    mapM_ (BL.appendFile fileOut) (map (unModFile . BL.take chunkSize) fs)
    let trueList = take (fileParts + 1) $ iterate id True
    if map BL.null fs == trueList
        then putStrLn "Unsplitting Done"
        else unsplit' x (map (BL.drop chunkSize) fs) 
