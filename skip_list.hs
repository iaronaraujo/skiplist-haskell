module MySkipList
  where

-- value level next down
data SkipListNode = Nil | SkipListNodeConstructor Int Int SkipListNode SkipListNode
  deriving (Eq, Show)


getValue (SkipListNodeConstructor value level next down) = value
getDown (SkipListNodeConstructor value level next down) = down
getNext (SkipListNodeConstructor value level next down) = next
getNext Nil = Nil

addElement element lv (SkipListNodeConstructor value level next down)
  | (level > lv) && (next /= Nil) && (element > getValue next) = (SkipListNodeConstructor value level (addElement element lv next) (addElement element lv down))
  | level > lv = (SkipListNodeConstructor value level next (addElement element lv down))
  | level < lv = (SkipListNodeConstructor element lv Nil (addElement element (lv-1) (SkipListNodeConstructor value level next down)))
  | otherwise = addElementOnLv element lv (SkipListNodeConstructor value level next down)

addElementOnLv element lv Nil = Nil
addElementOnLv element lv (SkipListNodeConstructor value level next down)
  | element < value = (SkipListNodeConstructor element lv current nextDown)
  | (next == Nil || element <= (getValue next)) = (SkipListNodeConstructor value level (SkipListNodeConstructor element lv next (searchInLine element nextDown)) nextDown)
  | otherwise =  (SkipListNodeConstructor value level (addElementOnLv element lv next) nextDown)
  where current = (SkipListNodeConstructor value level next down)
        nextDown = addElementOnLv element (lv-1) down

createNodeDownFirst value level down next = (SkipListNodeConstructor value level next down)

searchInLine element Nil = Nil
searchInLine element (SkipListNodeConstructor value level next down)
  | value == element = (SkipListNodeConstructor value level next down)
  | otherwise = searchInLine element next

createSkipList value 0 = SkipListNodeConstructor value 0 Nil Nil
createSkipList value height = SkipListNodeConstructor value height Nil (createSkipList value (height -1))

printSkipList (SkipListNodeConstructor value level next down)
  | down == Nil = [(getSkipListLine (SkipListNodeConstructor value level next down))]
  | otherwise = [(getSkipListLine (SkipListNodeConstructor value level next down))] ++ printSkipList down

getSkipListLine (SkipListNodeConstructor value level next down)
  | next == Nil = [value]
  | otherwise = [value] ++ getSkipListLine next

findNode element Nil = Nil
findNode element (SkipListNodeConstructor value level next down)
  | element == value = (SkipListNodeConstructor value level next down)
  | ((next /= Nil) && (element >= (getValue next))) = findNode element next
  | otherwise = findNode element down

findElement element sl
  | result == Nil = Nothing
  | otherwise = Just (getValue result)
  where result = findNode element sl

removeElement element Nil = Nil
removeElement element (SkipListNodeConstructor value level next down)
  | ((next /= Nil) && (element == (getValue next))) = removeElement element slWithoutElement
  | ((next /= Nil) && (element >= (getValue next))) = (SkipListNodeConstructor value level revNext revDown)
  | otherwise = (SkipListNodeConstructor value level next revDown)
  where slWithoutElement = (SkipListNodeConstructor value level (getNext next) down)
        revDown = (removeElement element down)
        revNext = (removeElement element next)

removeElementFromSkipList element (MySLConstructor currentListHeight head) = (MySLConstructor currentListHeight (removeElement element head))
-- removeElementFromSkipList element (MySLConstructor currentListHeight (SkipListNodeConstructor value level next down))
--  | element == (getValue head) = (MySLConstructor currentListHeight (removeElement element (SkipListNodeConstructor value level next down)))
--  | otherwise = (MySLConstructor currentListHeight (removeElement element (SkipListNodeConstructor value level next down)))

getDirectNext (SkipListNodeConstructor value level next down)
  | ((level == 0) && (next /= Nil)) = (getValue next)
  | ((level == 0) && (next == Nil)) = 0
  | otherwise = getDirectNext down

-- currentListHeight head
data MySkipList = MySLConstructor Int SkipListNode
  deriving (Eq, Show)

createMySkipList (SkipListNodeConstructor value level next down) = MySLConstructor (level+1) (SkipListNodeConstructor value level next down)

increaseHeight (MySLConstructor currentListHeight (SkipListNodeConstructor value level next down)) = (MySLConstructor (currentListHeight+1) (SkipListNodeConstructor value (level+1) Nil current))
  where current = (SkipListNodeConstructor value level next down)

-- Treats the add of an element higher in height than Head.
addHighElementSL element height (MySLConstructor currentListHeight head)
  | (height > currentListHeight) && (element >= (getValue head)) = addHighElementSL element height (increaseHeight (MySLConstructor currentListHeight head))
  | (height > currentListHeight) && (element < (getValue head)) = MySLConstructor height putElem
  | otherwise = MySLConstructor currentListHeight putElem
  where putElem = (addElement element (height-1) head)

-- Treats the add of an element lower than the head but with lower height; current standard add.
addElementToSkipList element height (MySLConstructor currentListHeight (SkipListNodeConstructor value level next down))
  | (element < value) && (height < currentListHeight) = addHighElementSL element currentListHeight mySL
  | otherwise = addHighElementSL element height mySL
    where mySL = MySLConstructor currentListHeight (SkipListNodeConstructor value level next down)

getHeight (MySLConstructor currentListHeight head) = currentListHeight
getHead (MySLConstructor currentListHeight head) = head

-- instance Show SkipListNode where
--  show Nil = "Nil"
--  show (SkipListNodeConstructor value level next down) = show (printSkipList (SkipListNodeConstructor value level next down))
