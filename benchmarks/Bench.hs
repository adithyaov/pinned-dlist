import Data.Pinned.DList
import Control.Monad
import System.CPUTime

main = do
  print "Timing DList"
  dl <- foldM (flip consIO) empty (replicate 2000000 1 :: [Int])
  print =<< time (foldrIO (+) (0 :: Int) dl)
  print =<< time (foldrIO (-) (0 :: Int) dl)

  print "Timing List"
  let l = foldl (flip (:)) [] (replicate 2000000 1 :: [Int]) 
  print =<< time (return $! foldr (+) (0 :: Int) l)
  print =<< time (return $! foldr (-) (0 :: Int) l)


time :: IO t -> IO (t, Double)
time a = do
  start <- getCPUTime
  v <- a
  end   <- getCPUTime
  let diff = fromIntegral (end - start)
  return (v, diff :: Double)
