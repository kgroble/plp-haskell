
import BentleyOttmannSpec
import RedBlackTreeSpec
import Test.HUnit

tests :: [Test]
tests = testContains ++
        testInsert ++
        testRemove ++
        testSmallest ++
        testIntersection

main :: IO Counts
main = runTestTT $ TestList tests
