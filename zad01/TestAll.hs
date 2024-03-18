import qualified TestSet
import qualified TestBasicGraph
import qualified TestRelation
import qualified TestFibonacci
import qualified TestKwasow

main = do
  writeln ">>> [Official] Set"
  TestSet.main
  writeln "\n\n>>> [Official] Basic"
  TestBasicGraph.main
  writeln "\n\n>>> [Official] Relation"
  TestRelation.main
  writeln "\n\n>>> Fibonacci"
  TestFibonacci.main
  writeln "\n\n>>> Kwasow"
  TestKwasow.main

writeln :: String -> IO ()
writeln = putStrLn
