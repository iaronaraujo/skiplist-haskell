module MySkipList
  where

-- value level next down
data SkipListNode = Nil | SkipListNodeConstructor Int Int SkipListNode SkipListNode
  deriving (Eq, Show)

-- funcoes auxiliares
getValue (SkipListNodeConstructor value level next down) = value
getDown (SkipListNodeConstructor value level next down) = down
getNext (SkipListNodeConstructor value level next down) = next
getNext Nil = Nil

-- adiciona elemento em um SkipListNode
addElement element lv (SkipListNodeConstructor value level next down)
  | (level > lv) && (next /= Nil) && (element > getValue next) = (SkipListNodeConstructor value level (addElement element lv next) (addElement element lv down))
  | level > lv = (SkipListNodeConstructor value level next (addElement element lv down))
  | level < lv = (SkipListNodeConstructor element lv Nil (addElement element (lv-1) (SkipListNodeConstructor value level next down)))
  | otherwise = addElementOnLv element lv (SkipListNodeConstructor value level next down)

-- adiciona elemento em um SkipListNode quando ja se sabe que esta no nivel/altura certa
addElementOnLv element lv Nil = Nil
addElementOnLv element lv (SkipListNodeConstructor value level next down)
  | element < value = (SkipListNodeConstructor element lv current nextDown)
  | (next == Nil || element <= (getValue next)) = (SkipListNodeConstructor value level (SkipListNodeConstructor element lv next (searchInLine element nextDown)) nextDown)
  | otherwise =  (SkipListNodeConstructor value level (addElementOnLv element lv next) nextDown)
  where current = (SkipListNodeConstructor value level next down)
        nextDown = addElementOnLv element (lv-1) down

-- procura um elemento em uma determinada altura
searchInLine element Nil = Nil
searchInLine element (SkipListNodeConstructor value level next down)
  | value == element = (SkipListNodeConstructor value level next down)
  | otherwise = searchInLine element next

-- cria um SkipListNode
createSkipListNode value 0 = SkipListNodeConstructor value 0 Nil Nil
createSkipListNode value height = SkipListNodeConstructor value height Nil (createSkipListNode value (height -1))

-- imprime um node como listas de lista
printSkipList (SkipListNodeConstructor value level next down)
  | down == Nil = [(getSkipListLine (SkipListNodeConstructor value level next down))]
  | otherwise = [(getSkipListLine (SkipListNodeConstructor value level next down))] ++ printSkipList down

-- auxiliar para a funcao acima: imprime uma altura
getSkipListLine (SkipListNodeConstructor value level next down)
  | next == Nil = [value]
  | otherwise = [value] ++ getSkipListLine next

-- imprime uma SkipList como lista de tuplas, onde o primeiro elemento eh o value, e o segundo sua altura
printSkipListAsTuples (MySLConstructor currentListHeight head) = (printNodeAsTuple (head) (head))

-- auxiliar para a funcao acima: faz o passo recursivo necessario para imprimir os valores corretamente
printNodeAsTuple (SkipListNodeConstructor value level next down) head
  | nextLv0 == Nil = [(value, level)]
  | otherwise = [(value, level)] ++ (printNodeAsTuple (findNode (getValue nextLv0) head) head)
  where nextLv0 = getNextNode value (SkipListNodeConstructor value level next down)

-- auxiliar para a funcao acima: pega o proximo node de um certo node com um valor value no nivel 0
getNextNode value sl = getNext current_lv0
  where current = (findNode value sl)
        current_lv0 = getLowerLevel current

-- auxiliar para a funcao acima: pega o no mais abaixo do atual, com excecao do Nil
getLowerLevel (SkipListNodeConstructor value level next down)
  | level == 0 = (SkipListNodeConstructor value level next down)
  | otherwise = getLowerLevel down

-- procura por um Node
findNode element Nil = Nil
findNode element (SkipListNodeConstructor value level next down)
  | element == value = (SkipListNodeConstructor value level next down)
  | ((next /= Nil) && (element >= (getValue next))) = findNode element next
  | otherwise = findNode element down

-- procura por um Elemento
findElement element sl
  | result == Nil = Nothing
  | otherwise = Just (getValue result)
  where result = findNode element sl

-- remove um elemento
removeElement element Nil = Nil
removeElement element (SkipListNodeConstructor value level next down)
  | ((next /= Nil) && (element == (getValue next))) = removeElement element slWithoutElement
  | ((next /= Nil) && (element >= (getValue next))) = (SkipListNodeConstructor value level revNext revDown)
  | otherwise = (SkipListNodeConstructor value level next revDown)
  where slWithoutElement = (SkipListNodeConstructor value level (getNext next) down)
        revDown = (removeElement element down)
        revNext = (removeElement element next)

-- remove elemento de uma skiplist
removeElementFromSkipList element (MySLConstructor currentListHeight (SkipListNodeConstructor value level next down))
  | element == (getValue head) = (MySLConstructor currentListHeight (removeHead (getDirectNext head) head))
  | otherwise = (MySLConstructor currentListHeight (removeElement element head))
  where head = (SkipListNodeConstructor value level next down)

-- auxiliar para a funcao acima: remove o head de uma skiplist
removeHead element (SkipListNodeConstructor value level next down)
  | ((next /= Nil) && (element == (getValue next))) = next
  | otherwise = (SkipListNodeConstructor element level next (removeHead element down))

-- pega o proximo elemento no nivel mais abaixo do node atual
getDirectNext (SkipListNodeConstructor value level next down)
  | ((level == 0) && (next /= Nil)) = (getValue next)
  | ((level == 0) && (next == Nil)) = 0
  | otherwise = getDirectNext down

-- currentListHeight head
data MySkipList = MySLConstructor Int SkipListNode
  deriving (Eq, Show)

-- cria skiplist
createMySkipList (SkipListNodeConstructor value level next down) = MySLConstructor (level+1) (SkipListNodeConstructor value level next down)

-- aumenta a altura de uma skiplist
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
