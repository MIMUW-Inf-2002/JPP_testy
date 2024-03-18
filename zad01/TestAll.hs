import qualified TestSet
import qualified TestBasicGraph
import qualified TestRelation
import qualified TestFibonacci


main = do
  writeln "Set"
  TestSet.main
  writeln "Basic"
  TestBasicGraph.main
  writeln "Relation"
  TestRelation.main
  writeln "Fibonacci"
  TestFibonacci.main

writeln :: String -> IO ()
writeln = putStrLn
