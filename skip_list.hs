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

-- instance Show SkipListNode where
--  show Nil = "Nil"
--  show (SkipListNodeConstructor value level next down) = show (printSkipList (SkipListNodeConstructor value level next down))
