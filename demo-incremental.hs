module Main where

import Data.List (foldl')
import Incremental

main :: IO ()
main = do
  doPure "take the first ten integers" (take' 10 [0..])
  let s1 = "a string with 4 digits 123, yay!"
  doPure ("count digits in " ++ show s1) (countDigits s1)
  doIO "first 10 characters of file1.txt" (takeSource 10 file1)
  doIO "first 10 characters of file2.txt" (takeSource 10 file2)
  doIO "first 40 characters of file1.txt catted with file2.txt" $
    takeSource 40 $ file1 `cat` file2
  doIO ("count the digits in " ++ show zeroToNineString) $ digitsSource zeroToNine
  doIO "count the digits in file1" $ digitsSource file1
  doIO "count the digits in file2" $ digitsSource file2
  doIO "count the digits in file1.txt catted with file2.txt" $
    digitsSource (file1 `cat` file2)
  doIO ("count the digits in " ++ show zeroToNineString ++ " catted with file1 catted with file2") $
    digitsSource (zeroToNine `cat` file1 `cat` file2)
  doIO ("count the digits in " ++ show zeroToNineString ++ " catted with itself 10 times, and with file1") $
    digitsSource $ (foldl' cat empty (replicate 10 zeroToNine)) `cat` file1
  doIO "take the first character of file1" (finish file1 incHead)
  doIO "take the first two characters of file1" (finish file1 incTwoHeads)
  where file1 = fileCharSource "file1.txt"
        file2 = fileCharSource "file2.txt"
        zeroToNineString =  "0123456789"
        zeroToNine = listSource zeroToNineString

takeSource :: Monad m => Int -> Source m Char -> m (Either String String)
takeSource n s = finish s (incTake n)

digitsSource :: Monad m => Source m Char -> m (Either String Int)
digitsSource s = finish s incCountDigits

doPure :: Show a => String -> a -> IO ()
doPure msg v = putStr msg >> putStr " " >> print v

doIO :: Show a => String -> IO a -> IO ()
doIO msg = (>>= doPure msg)
