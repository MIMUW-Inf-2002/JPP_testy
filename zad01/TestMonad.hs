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
