-- value level next down
data SkipListNode = Nil | SkipListNodeConstructor Int Int SkipListNode SkipListNode
  deriving (Eq, Show)


getValue (SkipListNodeConstructor value level next down) = value

addElement element lv (SkipListNodeConstructor value level next down)
  | level > lv = addElement element lv down
  | otherwise = addElementOnLv element lv (SkipListNodeConstructor value level next down)

addElementOnLv element lv Nil = Nil
addElementOnLv element lv (SkipListNodeConstructor value level next down)
  | element < value = (SkipListNodeConstructor element lv current nextDown)
  | (next == Nil || element <= (getValue next)) = (SkipListNodeConstructor value level (SkipListNodeConstructor element lv next nextDown) down)
  | otherwise = addElementOnLv element lv next
  where current = (SkipListNodeConstructor value level next down)
        nextDown = addElementOnLv element (lv-1) down
