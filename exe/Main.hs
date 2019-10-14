module Main where

import Data.Pinned.DList
import Control.Monad

main :: IO ()
main = main1

main2 = print $ foldr (+) 0 $ foldl (flip (:)) [] (replicate 2000000 1 :: [Int])

main1 = print
    =<< foldrIO (+) 0
    =<< foldM (flip consIO) empty (replicate 2000000 1 :: [Int])



