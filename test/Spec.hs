import Entities
import Linear.V2 (V2 (..))
import Lens.Micro ((%~), (&), (.~), (^.))
import Lens.Micro.TH (makeLenses)
import Test.QuickCheck

import Data.Typeable
isInteger :: (Typeable a) => a -> Bool
isInteger n = typeOf n == typeOf (1::Int)

genV2 :: Gen (V2 Int)
genV2 = do
  x <- chooseInt (0, 1000)
  y <- chooseInt (0, 1000)
  return (V2 x y)

prop_getV2x :: Property
prop_getV2x = forAll genV2 (\v@(V2 x y) -> getV2x v == x)

prop_getV2y :: Property
prop_getV2y = forAll genV2 (\v@(V2 x y) -> getV2y v == y)

prop_incr :: Property
prop_incr = forAll genGame (\g -> (getTick g + 1) == (getTick (tickincr g)) || ((g ^. isOver) /= 1))

prop_obstaclePos :: Property
prop_obstaclePos = forAll genGame obstaclePosProp 

obstaclePosProp :: Game -> Bool
obstaclePosProp g 
  | (g ^. isOver) /= 1 = True
  | otherwise = checkObList (g ^. obstacleList)
    where 
      checkObList [] = True
      checkObList xs = foldr checkOnePos True xs
        where
          checkOnePos (pos, w) result = (((getV2x pos) > groundLength) && ((getV2x pos) < (groundLength + maxObstacleDistance))) && result

main :: IO ()
main = do
  quickCheck prop_getV2x
  quickCheck prop_getV2y
  quickCheck prop_incr
  quickCheck prop_obstaclePos
  putStrLn "Done"
