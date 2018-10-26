{-
• There are 5 houses that are each a different colour.
• There is a person of a different nationality in each house.
• The 5 owners drink a certain drink. They each smoke a certain brand of cigarettes and also have a certain pet. No owner has the same pet, smokes the same brand of cigarettes nor drinks the same drink.
• The question is. “Who has the fish?”

CLUES

1. The British man lives in the red house.
2. The Swedish man has a dog for a pet.
3. The Danish man drinks tea.
4. The green house is to the left of the white house.
5. The owner of the green house drinks coffee.
6. The person that smokes Pall Mall has a bird.
7. The owner of the yellow house smokes Dunhill.
8. The person that lives in the middle house drinks milk.
9. The Norwegian lives in the first house.
10. The person that smokes Blend, lives next to the one that has a cat.
11. The person that has a horse lives next to the one that smokes Dunhill.
12. The one that smokes Bluemaster drinks beer.
13. The German smokes Prince.
14. The Norwegian lives next to a blue house.
15. The person that smokes Blend, has a neighbour that drinks water.
-}

module Main where

import Control.Monad       (forM_, mzero, mplus)
import Control.Monad.State (MonadState)
import Data.List           (tails)
import Data.Map            (Map,(!))
import Data.Traversable    (sequence)
import Prelude hiding      (sequence,(&&),(||),and,or)
import Data.Proxy
import qualified Data.Map as Map
import Ersatz
import Ersatz.Solver.BDD

main :: IO ()
main = do 
  (Satisfied, Just solution) <- solveWith robdd puzzle
  forM_ people $ \who ->
    putStrLn (who ++ ": " ++ unwords (attributesForPerson solution who))

  (Unsatisfied, Proxy) <- solveWith robdd $ do
    p <- puzzle
    assert (encode solution /== p)

  return ()

-- | Return a list of the attributes matching a person. Note that
-- the solution is a 'Map' of 'Bool' because we use the completed
-- solution at this point rather than the 'Map' of 'Bit' we have
-- when computing the constraints.
attributesForPerson :: Map (String,String) Bool -> String -> [String]
attributesForPerson solution who
  = [ v
    | vs <- tail attributes -- skip people
    , v  <- vs
    , solution!key who v
    ]

attributes :: [[String]]
attributes = [people, colors, drinks, cigs, pets, houses]

people, colors, drinks, cigs, pets, houses :: [String]
people = ["brit"   ,"german","swede" ,"dane"    ,"norwegian" ]
colors = ["red"    ,"yellow","green" ,"blue"    ,"white"     ]
drinks = ["coffee" ,"beer"  ,"milk"  ,"tea"     ,"water"     ]
cigs   = ["dunhill","blend" ,"prince","pallmall","bluemaster"]
pets   = ["horse"  ,"dog"   ,"cat"   ,"bird"    ,"fish"      ]
houses = ["one"    ,"two"   ,"three" ,"four"    ,"five"      ]

-- | Construct a map of attribute associations for every combination
-- of attributes.
defineVariables ::
  (HasSAT s, MonadState s m, Variable a) =>
  m (Map (String,String) a)
defineVariables
  = sequence
  $ Map.fromList
    [ (key v1 v2, exists)
    | xs      <- subsequencesN 2 attributes
    , [v1,v2] <- sequence xs
    ]

-- Put the two attributes in a canonical ordering for forming a key.
key :: String -> String -> (String,String)
key x y
  | x <= y    = (x,y)
  | otherwise = (y,x)

-- | Return all subsequences of the given list with the given length.
subsequencesN :: Int -> [a] -> [[a]]
subsequencesN 0 _      = return []
subsequencesN _ []     = mzero
subsequencesN n (x:xs) = keepX `mplus` skipX
  where
  keepX = fmap (x:) (subsequencesN (n-1) xs)
  skipX =            subsequencesN  n    xs

-- | Add constraints that 1) each attribute is associated with
-- exactly one attribute from each of the other categories, and
-- 2) attribute associations are transitive.
generalConstraints :: Boolean a => Map (String,String) a -> a
generalConstraints v = uniqueness && transitivity
  where
  uniqueness
    = and [ unique a1 a2 &&
            unique a2 a1
          | [a1,a2] <- subsequencesN 2 attributes
          ]

  transitivity
    = and [ v!key v1 v2 ==> v!key v2 v3 ==> v!key v1 v3
          | xs         <- subsequencesN 3 attributes
          , [v1,v2,v3] <- sequence xs
          ]

  -- each y in ys is associated with exactly one x in xs
  unique xs ys
    = and [ singleton [ v!key x y | x <- xs ]
          | y <- ys
          ]

-- | Returns 'true' if and only if exactly one element in
-- the list is 'true'.
singleton :: Boolean a => [a] -> a
singleton xs = atLeastOne && atMostOne
  where
  atLeastOne = or xs
  atMostOne  = and [ y ==> nor ys | y:ys <- tails xs ]

-- | Return 'true' if and only if the first attribute
-- is associated with a house number directly left of
-- the house number of associated with the second attribute.
leftOf :: Boolean a => Map (String,String) a -> String -> String -> a
leftOf v l r
  = or [ v!key l x
      && v!key r y
       | x:y:_ <- tails houses
       ]

-- | Return 'true' if first attribute is "left of" second attribue
-- or visa versa.
nextTo :: Boolean a => Map (String,String) a -> String -> String -> a
nextTo v a b
   = leftOf v a b
  || leftOf v b a


-- | Check whether a particular assignment of the attribute associations
-- satisfies the clues for this puzzle.
validAssignment :: Boolean a => Map (String,String) a -> a
validAssignment v
   = and [ generalConstraints v            -- Preamble
         , v!key    "brit"       "red"     -- Clue  1
         , v!key    "swede"      "dog"     -- Clue  2
         , v!key    "dane"       "tea"     -- Clue  3
         , leftOf v "green"      "white"   -- Clue  4
         , v!key    "green"      "coffee"  -- Clue  5
         , v!key    "pallmall"   "bird"    -- Clue  6
         , v!key    "yellow"     "dunhill" -- Clue  7
         , v!key    "three"      "milk"    -- Clue  8
         , v!key    "norwegian"  "one"     -- Clue  9
         , nextTo v "blend"      "cat"     -- Clue 10
         , nextTo v "horse"      "dunhill" -- Clue 11
         , v!key    "bluemaster" "beer"    -- Clue 12
         , v!key    "german"     "prince"  -- Clue 13
         , nextTo v "norwegian"  "blue"    -- Clue 14
         , nextTo v "blend"      "water"   -- Clue 15
         ]


-- | Construct an assignment of attribute associations that satisfies
-- all of the constraints of the puzzle.
puzzle ::
  (MonadState s m, HasSAT s) =>
  m (Map (String,String) Bit)
puzzle =
  do v <- defineVariables
     assert (validAssignment v)
     return v
