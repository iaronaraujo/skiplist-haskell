import Test.HUnit
import MySkipList

h1 = MySkipList.createSkipList 3 2
h2 = MySkipList.addElement 5 2 h1
h3 = MySkipList.addElement 4 1 h2
h4 = MySkipList.addElement 1 2 h3
h5 = MySkipList.addElement 2 1 h4

a2 = MySkipList.addElement 4 1 h1
a3 = MySkipList.addElement 5 2 a2

b2 = MySkipList.addElement 1 3 h1

show_of_h5 = "SkipListNodeConstructor 1 2 (SkipListNodeConstructor 3 2 (SkipListNodeConstructor 5 2 Nil (SkipListNodeConstructor 5 1 Nil (SkipListNodeConstructor 5 0 Nil Nil))) (SkipListNodeConstructor 3 1 (SkipListNodeConstructor 4 1 (SkipListNodeConstructor 5 1 Nil (SkipListNodeConstructor 5 0 Nil Nil)) (SkipListNodeConstructor 4 0 (SkipListNodeConstructor 5 0 Nil Nil) Nil)) (SkipListNodeConstructor 3 0 (SkipListNodeConstructor 4 0 (SkipListNodeConstructor 5 0 Nil Nil) Nil) Nil))) (SkipListNodeConstructor 1 1 (SkipListNodeConstructor 2 1 (SkipListNodeConstructor 3 1 (SkipListNodeConstructor 4 1 (SkipListNodeConstructor 5 1 Nil (SkipListNodeConstructor 5 0 Nil Nil)) (SkipListNodeConstructor 4 0 (SkipListNodeConstructor 5 0 Nil Nil) Nil)) (SkipListNodeConstructor 3 0 (SkipListNodeConstructor 4 0 (SkipListNodeConstructor 5 0 Nil Nil) Nil) Nil)) (SkipListNodeConstructor 2 0 (SkipListNodeConstructor 3 0 (SkipListNodeConstructor 4 0 (SkipListNodeConstructor 5 0 Nil Nil) Nil) Nil) Nil)) (SkipListNodeConstructor 1 0 (SkipListNodeConstructor 2 0 (SkipListNodeConstructor 3 0 (SkipListNodeConstructor 4 0 (SkipListNodeConstructor 5 0 Nil Nil) Nil) Nil) Nil) Nil))"

test1 = TestCase (assertEqual "SkipList creation" [[3],[3],[3]] (MySkipList.printSkipList h1))
test2 = TestCase (assertEqual "Addition on H1" [[3,5],[3,5],[3,5]] (MySkipList.printSkipList h2))
test3 = TestCase (assertEqual "Addition on H2" [[3,5],[3,4,5],[3,4,5]] (MySkipList.printSkipList h3))
test4 = TestCase (assertEqual "Addition on H3" [[1,3,5],[1,3,4,5],[1,3,4,5]] (MySkipList.printSkipList h4))
test5 = TestCase (assertEqual "Addition on H4" [[1,3,5],[1,2,3,4,5],[1,2,3,4,5]] (MySkipList.printSkipList h5))
test6 = TestCase (assertEqual "Addition on H1" [[3],[3,4],[3,4]] (MySkipList.printSkipList a2))
test7 = TestCase (assertEqual "Addition on A2" [[3,5],[3,4,5],[3,4,5]] (MySkipList.printSkipList a3))
test8 = TestCase (assertEqual "Addition on H1 with height > maxheight" [[1,3],[1,3],[1,3]] (MySkipList.printSkipList b2))
test9 = TestCase (assertEqual "Test Show" show_of_h5 (show h5))

test10 = TestCase (assertEqual "FindElement 0" (Nothing) (MySkipList.findElement 0 h5))
test11 = TestCase (assertEqual "FindElement 1" (Just 1) (MySkipList.findElement 1 h5))
test12 = TestCase (assertEqual "FindElement 2" (Just 2) (MySkipList.findElement 2 h5))
test13 = TestCase (assertEqual "FindElement 3" (Just 3) (MySkipList.findElement 3 h5))
test14 = TestCase (assertEqual "FindElement 4" (Just 4) (MySkipList.findElement 4 h5))
test15 = TestCase (assertEqual "FindElement 5" (Just 5) (MySkipList.findElement 5 h5))
test16 = TestCase (assertEqual "FindElement 6" (Nothing) (MySkipList.findElement 6 h5))

test17 = TestCase (assertEqual "FindNode 0" (Nil) (MySkipList.findNode 0 h5))
test18 = TestCase (assertEqual "FindNode 1" [[1,3,5],[1,2,3,4,5],[1,2,3,4,5]] (MySkipList.printSkipList (MySkipList.findNode 1 h5)))
test19 = TestCase (assertEqual "FindNode 2" [[2,3,4,5],[2,3,4,5]] (MySkipList.printSkipList (MySkipList.findNode 2 h5)))
test20 = TestCase (assertEqual "FindNode 3" [[3,5],[3,4,5],[3,4,5]] (MySkipList.printSkipList (MySkipList.findNode 3 h5)))
test21 = TestCase (assertEqual "FindNode 4" [[4,5],[4,5]] (MySkipList.printSkipList (MySkipList.findNode 4 h5)))
test22 = TestCase (assertEqual "FindNode 5" [[5],[5],[5]] (MySkipList.printSkipList (MySkipList.findNode 5 h5)))
test23 = TestCase (assertEqual "FindNode 6" (Nil) (MySkipList.findNode 6 h5))

insertTests = TestList [test1, test2, test3, test4, test5, test6, test7, test8, test9]
findElementTests = TestList [test10, test11, test12, test13, test14, test15, test16]
findNodeTests = TestList [test17, test18, test19, test20, test21, test22, test23]

-- runTestTT tests
