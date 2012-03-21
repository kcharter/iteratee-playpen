module Main where

import Incremental

main :: IO ()
main = do
  doPure "take the first ten integers" (take' 10 [0..])
  let s1 = "a string with 4 digits 123, yay!"
  doPure ("count digits in " ++ show s1) (countDigits s1)

doPure :: Show a => String -> a -> IO ()
doPure msg v = putStr msg >> putStr " " >> print v