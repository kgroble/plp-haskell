import RedBlackTreeSpec
import Test.HUnit

tests :: [Test]
tests = testContains ++ testInsert ++ testRemove

main :: IO Counts
main = runTestTT $ TestList tests
