import BinaryTreeSpec
import Test.HUnit

tests :: [Test]
tests = containsTests

main :: IO Counts
main = runTestTT $ TestList tests
