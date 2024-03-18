module TestFibonacci where
import Graph
import Test.QuickCheck
import Set

fibHelper f a b n
    | n > 0 = fibHelper f b (f a b) (n-1)
    | otherwise = a  

fibAdd:: Int -> Relation Int
fibAdd =
    fibAddHelper 0 1
    where
        fibAddHelper:: Relation Int -> Relation Int -> Int -> Relation Int
        fibAddHelper = fibHelper (+)

fibMul:: Int -> Relation Int
fibMul =
    fibMulHelper 0 1
    where
        fibMulHelper:: Relation Int -> Relation Int -> Int -> Relation Int
        fibMulHelper = fibHelper (*)

fibAdditionSensible:: Bool
fibAdditionSensible = 
    fibAdd 35 == Relation (fromList [0,1]) (fromList [])

fibMultiplicationSensible:: Bool
fibMultiplicationSensible = 
    fibMul 19 == Relation (fromList [0,1]) (fromList [(0,0), (0,1), (1,0), (1,1)])


fibSortEfficient:: Bool
fibSortEfficient =
    helper [1..10000] 100 == [1..10000]
    where
        helper:: [Int] -> Int -> [Int]
        helper s n
            | n == 0 = s
            | otherwise = helper (toAscList (fromList s)) (n-1)


main:: IO()
main = do
        writeln "sorting efficient"
        quickCheck fibSortEfficient
        writeln "Note: these are NOT supposed to take a long time"
        writeln "addition sensible"
        quickCheck fibAdditionSensible
        writeln "multiplication sensible"
        quickCheck fibMultiplicationSensible

writeln :: String -> IO ()
writeln = putStrLn
