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

test1 = TestCase (assertEqual "SkipList creation" [[3],[3],[3]] (MySkipList.printSkipList h1))
test2 = TestCase (assertEqual "Addition on H1" [[3,5],[3,5],[3,5]] (MySkipList.printSkipList h2))
test3 = TestCase (assertEqual "Addition on H2" [[3,5],[3,4,5],[3,4,5]] (MySkipList.printSkipList h3))
test4 = TestCase (assertEqual "Addition on H3" [[1,3,5],[1,3,4,5],[1,3,4,5]] (MySkipList.printSkipList h4))
test5 = TestCase (assertEqual "Addition on H4" [[1,3,5],[1,2,3,4,5],[1,2,3,4,5]] (MySkipList.printSkipList h5))
test6 = TestCase (assertEqual "Addition on H1" [[3],[3,4],[3,4]] (MySkipList.printSkipList a2))
test7 = TestCase (assertEqual "Addition on A2" [[3,5],[3,4,5],[3,4,5]] (MySkipList.printSkipList a3))
test8 = TestCase (assertEqual "Addition on H1 with height > maxheight" [[1,3],[1,3],[1,3]] (MySkipList.printSkipList b2))


tests = TestList [test1, test2, test3, test4, test5, test6, test7, test8]
ftests = TestList [test7]

-- runTestTT tests