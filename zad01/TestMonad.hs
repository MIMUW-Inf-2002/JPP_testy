import Test.QuickCheck
import Graph

monadRightId :: Basic Int -> Bool
monadRightId x = (x >>= return) == x

monadLeftId :: Int -> (Fun Int (Basic Int)) -> Bool
monadLeftId x (Fun _ g) = (return x >>= g) == g x

monadAssocProp :: Basic Int -> (Fun Int (Basic Int)) -> (Fun Int (Basic Int)) -> Bool
monadAssocProp x (Fun _ f) (Fun _ g) = ((x >>= f) >>= g) == (x >>= (\x' -> f x' >>= g))

monadAssocProp' :: Basic Int -> Basic Int -> Basic Int -> Bool
monadAssocProp' x y z = ((x >> y) >> z) == (x >> (y >> z))


main = do
       writeln "monad"
       quickCheck monadRightId
       quickCheck monadLeftId
       quickCheckWith stdArgs {maxSuccess = 10} monadAssocProp'

writeln :: String -> IO ()
writeln = putStrLn

-- instance (Arbitrary a, Graph g) => Arbitrary (g a) where
instance (Arbitrary a) => Arbitrary (Basic a) where
  arbitrary = sized arb where
    arb 0 = return empty
    arb 1 = vertex <$> arbitrary
    arb n = oneof [ union <$> arb2 <*> arb2
                  , connect <$> arb' <*> arb'] where
      arb2 = arb (div n 4)
      arb' = arb (intSqrt n)
      intSqrt :: Int -> Int
      intSqrt = round . sqrt . fromIntegral

