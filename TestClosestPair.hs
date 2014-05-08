import Test.HUnit
import ClosestPair

list1 = [(0,2),(1,2),(3,4),(7,8),(18,22),(1,5),(0,4),(0,8),(4,7),(9,8),(11,22)]
list2 = take ((length list1) - 1) list1
list3 = take ((length list2) - 1) list2
list4 = (7,8):list1
list5 = (7,7):list1
list6 = (7,9):list1
list7 = drop 1 list1

test1 = TestCase $ assertEqual ""
  [(0,2),(1,2)] (closest_pair list1)
test2 = TestCase $ assertEqual ""
  [(0,2),(1,2)] (closest_pair list2)
test3 = TestCase $ assertEqual ""
  [(0,2),(1,2)] (closest_pair list3)
test4 = TestCase $ assertEqual ""
  [(7,8),(7,8)] (closest_pair list4)
test5 = TestCase $ assertEqual ""
  [(0,2),(1,2)] (closest_pair list5)
test6 = TestCase $ assertEqual ""
  [(0,2),(1,2)] (closest_pair list6)
test7 = TestCase $ assertEqual ""
  [(0,4),(1,5)] (closest_pair list7)

tests = 
  TestList [TestLabel "test1" test1,
            TestLabel "test2" test2,
            TestLabel "test3" test3,
            TestLabel "test4" test4,
            TestLabel "test5" test5,
            TestLabel "test6" test6,
            TestLabel "test7" test7]

main = runTestTT tests
