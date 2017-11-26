import Ersatz
import Ersatz.Solver.BDD
import Control.Monad
import Control.Monad.State

problem :: (MonadState s m, HasSAT s) => m (Bits, Bits, Bits)
problem = do
  a <- liftM Bits (replicateM 5 exists)
  b <- liftM Bits (replicateM 5 exists)
  let c = a * b
  assert (a /== encode   1)
  assert (b /== encode   1)
  assert (c === encode 143)
  return (a,b,c)

main :: IO ()
main = do
  putStrLn "Solution:"
  (Satisfied, Just (a,b,c)) <- solveWith robdd problem
  putStrLn (show a ++ " * " ++ show b ++ " = " ++ show c)
