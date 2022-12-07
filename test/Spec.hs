import Entities
import Linear.V2 (V2 (..))
import Test.QuickCheck

genV2 :: Gen (V2 Int)
genV2 = do
  x <- chooseInt (0, 1000)
  y <- chooseInt (0, 1000)
  return (V2 x y)

prop_getV2x :: Property
prop_getV2x = forAll genV2 (\v@(V2 x y) -> getV2x v == x)

prop_getV2y :: Property
prop_getV2y = forAll genV2 (\v@(V2 x y) -> getV2x v == y)

-- prop_incr :: Property
-- prop_incr = forAll genGame (\g -> ((getTick g) + 1) == (getTick (tickincr g)))

main :: IO ()
main = do
  quickCheck prop_getV2x
  quickCheck prop_getV2y
  -- quickCheck prop_incr
  putStrLn "Done"
