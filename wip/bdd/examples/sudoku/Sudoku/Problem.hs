{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
module Sudoku.Problem (problem, range) where

import Prelude hiding ((&&), (||), not, and, or, all, any)

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Monad.Reader
import Control.Monad.State
import Data.Array (Array, (!))
import qualified Data.Array as Array
import Data.Word
import Ersatz

import Sudoku.Cell

type Index = (Word8,Word8)

type Grid = Array Index Cell

data Env = Env { envCellArray :: Grid    -- ^ The puzzle.
               , envValues    :: [Cell]  -- ^ The possible values for any cell.
               }
  deriving Show

problem :: (Applicative m, MonadState s m, HasSAT s)
        => Array Index Word8 -> m Grid
problem initValues = do
  cellArray <-  Array.listArray range
            <$> replicateM (Array.rangeSize range) exists

  runReaderT problem' $ Env cellArray (map encode [1..9])

  -- Assert all initial values.
  forM_ (Array.assocs initValues) $ \(idx, val) ->
    when (1 <= val && val <= 9) $
      assert $ (cellArray ! idx) === encode val

  return cellArray

problem' :: (MonadState s m, HasSAT s) => ReaderT Env m ()
problem' = do
  legalValues
  mapM_ allDifferent (subsquares ++ horizontal ++ vertical)

-- | Assert that each cell must have one of the legal values.
legalValues :: (MonadState s m, HasSAT s) => ReaderT Env m ()
legalValues = mapM_ legalValue . Array.elems =<< asks envCellArray
  where
    legalValue cell = do
      values <- asks envValues
      assert $ any (cell ===) values

-- | Assert that each cell in a group must have a different value.
allDifferent :: (MonadState s m, HasSAT s)
             => [(Word8,Word8)] -> ReaderT Env m ()
allDifferent indices = do
  cellArray <- asks envCellArray
  let pairs = [ (cellArray ! a, cellArray ! b)
              | a <- indices, b <- indices, a /= b
              ]
  forM_ pairs $ \(cellA, cellB) -> assert (cellA /== cellB)

-- | The valid index range for the grid.
range :: (Index,Index)
range = ((0,0),(8,8))

subsquares, horizontal, vertical :: [[Index]]

-- | The index group for each subsquare.
subsquares = do
  sqY <- [0..2]
  sqX <- [0..2]
  let top  = 3*sqY
      left = 3*sqX
  return [ (y,x) | y <- [top..top+2], x <- [left..left+2] ]

-- | The index group for each line.
horizontal = do
  line <- [0..8]
  return [ (line,x) | x <- [0..8] ]

-- | The index group for each column.
vertical = do
  column <- [0..8]
  return [ (y,column) | y <- [0..8] ]
