import RedBlackTreeSpec
import Test.HUnit

tests :: [Test]
tests = testContains ++ testInsert

main :: IO Counts
main = runTestTT $ TestList tests
