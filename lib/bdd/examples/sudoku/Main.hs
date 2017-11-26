{-# LANGUAGE CPP #-}
module Main (main) where

import Prelude hiding ((&&), (||), not, and, or, all, any)

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Monad
import Data.Array (Array, (!))
import qualified Data.Array as Array
import Data.List
import Data.Word
import Ersatz
import Ersatz.Solver.BDD
import Sudoku.Problem

main :: IO ()
main = do
  putStrLn "Problem:"
  putStr (render initValues)

  putStrLn "Solution:"
  (res, msol) <- solveWith robdd (problem initValues)
  when (res /= Satisfied) (fail (show res))
  case msol of
    Just sol -> putStr (render sol)
    _ -> fail ("sol was " ++ show msol)

initValues :: Array (Word8,Word8) Word8
initValues =
  -- From https://en.wikipedia.org/w/index.php?title=Sudoku&oldid=543290082
  Array.listArray range
    [ 5, 3, 0, 0, 7, 0, 0, 0, 0
    , 6, 0, 0, 1, 9, 5, 0, 0, 0
    , 0, 9, 8, 0, 0, 0, 0, 6, 0
    , 8, 0, 0, 0, 6, 0, 0, 0, 3
    , 4, 0, 0, 8, 0, 3, 0, 0, 1
    , 7, 0, 0, 0, 2, 0, 0, 0, 6
    , 0, 6, 0, 0, 0, 0, 2, 8, 0
    , 0, 0, 0, 4, 1, 9, 0, 0, 5
    , 0, 0, 0, 0, 8, 0, 0, 7, 9
    ]

render :: Array (Word8,Word8) Word8 -> String
render sol = unlines . renderGroups top divider bottom
           $ map (renderLine sol) [0..8]
  where
    top     = bar "┌" "───────" "┬" "┐"
    divider = bar "├" "───────" "┼" "┤"
    bottom  = bar "└" "───────" "┴" "┘"

    bar begin fill middle end =
      begin ++ intercalate middle (replicate 3 fill) ++ end

renderLine :: Array (Word8,Word8) Word8 -> Word8 -> String
renderLine sol y = unwords . renderGroups "│" "│" "│"
                 $ map (\x -> showN (sol ! (y,x))) [0..8]
  where
    showN n | 1 <= n && n <= 9 = show n
            | otherwise        = " "

renderGroups :: a -> a -> a -> [a] -> [a]
renderGroups begin middle end values =
  [begin] ++ intercalate [middle] (chunks 3 values) ++ [end]
  where
    chunks n = unfoldr $ \xs -> splitAt n xs <$ guard (not (null xs))
