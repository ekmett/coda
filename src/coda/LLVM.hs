module LLVM
  ( fun
  ) where

import Data.Foldable (toList)
import Data.Traversable (mapAccumL)
import LLVM.AST
import LLVM.IRBuilder as IR

-- | A slightly safer version of 'IR.function' using the encoding trick
-- from the @ad@ package
fun
  :: (MonadModuleBuilder m, Traversable f)
  => Name  -- ^ Function name
  -> f (Type, ParameterName)  -- ^ Parameter types and name suggestions
  -> Type  -- ^ Return type
  -> (f Operand -> IRBuilderT m ())  -- ^ Function body builder
  -> m Operand
fun n as r f = IR.function n (toList as) r $ \xs -> f (refill xs as)

-- | assumes list length and container length match
refill :: Traversable f => [a] -> f b -> f a
refill = fmap snd . mapAccumL (\(a:as') _ -> (as', a))
