{-# LANGUAGE TypeApplications #-}
module TestKwasow where
import Graph
import Test.QuickCheck
import Set

suite1 :: IO ()
suite1 = do
  quickCheck prop1
  quickCheck prop2
  quickCheck prop3
  quickCheck prop4
  quickCheck prop5
  quickCheck prop6
  quickCheck prop7
  quickCheck prop8 where
    prop1 = show example34 == "edges [(1,2),(2,3),(2,4),(3,5),(4,5)] + vertices [17]"
    prop2 = show (vertex @Basic 1) == "edges [] + vertices [1]"
    prop3 = show (connect @Basic 1 2) == "edges [(1,2)] + vertices []"
    prop4 = show (vertex @Relation 1) == "Relation {domain = [1], relation = []}"
    prop5 = show (connect @Relation 1 2) == "Relation {domain = [1,2], relation = [(1,2)]}"
    prop6 = todot example34 == "digraph {\n1 -> 2;\n2 -> 3;\n2 -> 4;\n3 -> 5;\n4 -> 5;\n17;\n}"
    prop7 = todot (vertex @Basic 1) == "digraph {\n1;\n}"
    prop8 = todot (connect @Basic 1 2) == "digraph {\n1 -> 2;\n}"

suite2 :: IO ()
suite2 = do
  quickCheck prop1 where
    l = [1..100000] ++ [1..100000] ++ [1..100000] ++ [1..100000] ++ [1..100000]
    prop1 = toAscList (fromList l) == [1..100000]

suite3 :: IO ()
suite3 = do
  quickCheck prop1
  quickCheck prop2
  quickCheck prop3
  quickCheck prop4
  quickCheck prop5
  quickCheck prop6 where
    prop1 = mergeV 3 4 34 example34 == 1*2 + 2*34 + 34*5 + 17
    prop2 = mergeV 4 3 34 example34 == 1*2 + 2*34 + 34*5 + 17
    prop3 = mergeV 2 6 34 example34 == 1*34 + 34*(3+4) + (3+4)*5 + 17
    prop4 = mergeV 6 2 34 example34 == 1*34 + 34*(3+4) + (3+4)*5 + 17
    prop5 = mergeV 21 37 34 example34 == example34
    prop6 = mergeV 1 2 12 (1*2) == (12*12)

suite4 :: IO ()
suite4 = do
  quickCheck prop1
  quickCheck prop2 where
    prop1 = splitV 34 3 4 (mergeV 3 4 34 example34) == 1*2 + 2*(4+3) + (4+3)*5 + 17
    prop2 = splitV 21 2 1 example34 == example34

-- Main
main :: IO ()
main = do
      -- Begin
      writeln "Starting Kwasow tests"

      -- Test suite 1
      writeln "[1/4] show + todot [<1s]"
      suite1

      -- Test suite 3
      writeln "[2/4] Sort long list (many repeat) [~1s]"
      suite2

      -- Test suite 4
      writeln "[3/4] mergeV"
      suite3

      -- Test suite 5
      writeln "[4/4] splitV"
      suite4

      writeln "Kwasow tests done"

-- Helper functions
writeln :: String -> IO ()
writeln = putStrLn
