module Main where

import           Calculator (calculator)

main :: IO ()
main = loop
  where
    loop = do
      input <- getLine
      putStrLn $ calculator input
      loop
