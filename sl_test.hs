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
test8 = TestCase (assertEqual "Addition on H1 with height > maxheight" [[1],[1,3],[1,3],[1,3]] (MySkipList.printSkipList b2))
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

l1 = MySkipList.createMySkipList h5
l2 = MySkipList.increaseHeight l1
r1 = MySkipList.createMySkipList h2
r2 = MySkipList.addHighElementSL 6 4 r1
r3 = MySkipList.addElementToSkipList 1 3 r1
r4 = MySkipList.addElementToSkipList 1 1 r1
r5 = MySkipList.addElementToSkipList 1 6 r1

test24_0 = TestCase (assertEqual "Test show of MySkipList" ("MySLConstructor 3 (" ++ show_of_h5 ++ ")") (show l1))
test25_1 = TestCase (assertEqual "Test get height l1" (3) (getHeight l1))
test26_2 = TestCase (assertEqual "Test get head l1" (h5) (getHead l1))
test27_3 = TestCase (assertEqual "Test of raw increaseHeight on l1" (4) (getHeight l2))
test28_4 = TestCase (assertEqual "Test of raw increaseHeight on l1: head change" (SkipListNodeConstructor 1 3 Nil h5) (getHead l2))
test29_5 = TestCase (assertEqual "Test addHighElementSL height change" (4) (getHeight r2))
test30_6 = TestCase (assertEqual "Test node change on addHighElementSL" [[3, 6], [3, 5, 6], [3, 5, 6], [3, 5, 6]] (printSkipList (getHead r2)))
test31_7 = TestCase (assertEqual "Test head change on addElementToSkipList" [[1, 3, 5], [1, 3, 5], [1, 3, 5]] (printSkipList (getHead r3)))
test32_8 = TestCase (assertEqual "Test head change on addElementToSkipList + new node's height change" [[1, 3, 5], [1, 3, 5], [1, 3, 5]] (printSkipList (getHead r4)))
test33_9 = TestCase (assertEqual "Test head change on addElementToSkipList + new list's structure change" [[1], [1], [1], [1, 3, 5], [1, 3, 5], [1, 3, 5]] (printSkipList (getHead r5)))
test34_10 = TestCase (assertEqual "Test new list's height change on addElementToSkipList" (6) (getHeight r5))

z1 = removeElement 1 h5
z2 = removeElement 2 h5
z3 = removeElement 3 h5
z4 = removeElement 4 h5
z5 = removeElement 5 h5
z6 = removeElementFromSkipList 1 l1
z7 = removeElementFromSkipList 2 z6

test35 = TestCase (assertEqual "RemoveElement 1 h5" [[1,3,5],[1,2,3,4,5],[1,2,3,4,5]] (MySkipList.printSkipList z1))
test36 = TestCase (assertEqual "RemoveElement 2 h5" [[1,3,5],[1,3,4,5],[1,3,4,5]] (MySkipList.printSkipList z2))
test37 = TestCase (assertEqual "RemoveElement 3 h5" [[1,5],[1,2,4,5],[1,2,4,5]] (MySkipList.printSkipList z3))
test38 = TestCase (assertEqual "RemoveElement 4 h5" [[1,3,5],[1,2,3,5],[1,2,3,5]] (MySkipList.printSkipList z4))
test39 = TestCase (assertEqual "RemoveElement 5 h5" [[1,3],[1,2,3,4],[1,2,3,4]] (MySkipList.printSkipList z5))
test40 = TestCase (assertEqual "RemoveElementFromSkipList 1 l1" [[2,3,5],[2,3,4,5],[2,3,4,5]] (printSkipList (getHead z6)))
test41 = TestCase (assertEqual "RemoveElementFromSkipList 2 z6" [[3,5],[3,4,5],[3,4,5]] (printSkipList (getHead z7)))

sl1 = MySkipList.createMySkipList h1
sl2 = MySkipList.createMySkipList h2
sl3 = MySkipList.createMySkipList h3
sl4 = MySkipList.createMySkipList h4
sl5 = MySkipList.createMySkipList h5

test42 = TestCase (assertEqual "printSkipListAsTuples sl1" [(3,2)] (printSkipListAsTuples sl1))
test43 = TestCase (assertEqual "printSkipListAsTuples sl2" [(3,2),(5,2)] (printSkipListAsTuples sl2))
test44 = TestCase (assertEqual "printSkipListAsTuples sl3" [(3,2),(4,1),(5,2)] (printSkipListAsTuples sl3))
test45 = TestCase (assertEqual "printSkipListAsTuples sl4" [(1,2),(3,2),(4,1),(5,2)] (printSkipListAsTuples sl4))
test46 = TestCase (assertEqual "printSkipListAsTuples sl5" [(1,2),(2,1),(3,2),(4,1),(5,2)] (printSkipListAsTuples sl5))

removeTests = TestList [test35, test36, test37, test38, test39, test40, test41]
mSLTest = TestList [test24_0, test25_1, test26_2, test27_3, test28_4, test29_5, test30_6, test31_7, test32_8, test33_9, test34_10]
printSLtests = TestList [test42, test43, test44, test45, test46]
allTests = TestList [test1, test2, test3, test4, test5, test6, test7, test8, test9, test10, test11, test12, test13, test14, test15, test16, test17, test18, test19, test20, test21, test22, test23, test24_0, test25_1, test26_2, test27_3, test28_4, test29_5, test30_6, test31_7, test32_8, test33_9, test34_10, test35, test36, test37, test38, test39, test40, test41, test42, test43, test44, test45, test46]


-- runTestTT allTests
