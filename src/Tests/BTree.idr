module Tests.BTree

import BTree
import Test.Unit.Assertions


testInsertMember : IO ()
testInsertMember =
  let tree = insert 1 empty in
  let tree = insert 5 tree in
  let tree = insert 3 tree in
  do
    assertTrue $ member 1 tree
    assertTrue $ member 5 tree
    assertTrue $ member 3 tree
    pure ()


testToTreeMember : IO ()
testToTreeMember =
  let tree = toTree [1, 5, 3] in
  do
    assertTrue $ member 1 tree
    assertTrue $ member 5 tree
    assertTrue $ member 3 tree
    pure ()

testToTreeToList : IO ()
testToTreeToList =
  let tree = toTree [1, 5, 3] in
  let list = toList tree in
  do
    assertEquals list [1, 3, 5]
    pure ()

testSplit1 : IO ()
testSplit1 =
  let tree = toTree [1, 5, 3, 2, 4] in
  let (l, v ,r) = split 3 tree in
  do
    assertEquals (toList l) [1, 2]
    assertEquals v (Just 3)
    assertEquals (toList r) [4, 5]
    pure ()

testSplit2 : IO ()
testSplit2 =
  let tree = toTree [1, 5, 2, 4] in
  let (l, v ,r) = split 3 tree in
  do
    assertEquals (toList l) [1, 2]
    assertEquals v Nothing
    assertEquals (toList r) [4, 5]
    pure ()

testSplit3 : IO ()
testSplit3 =
  let tree = toTree [1, 5, 2, 4] in
  let (l, v ,r) = split 6 tree in
  do
    assertEquals (toList l) [1, 2, 4, 5]
    assertEquals v Nothing
    assertEquals (toList r) []
    pure ()

testSplit4 : IO ()
testSplit4 =
  let tree = toTree [1, 5, 2, 4] in
  let (l, v ,r) = split 0 tree in
  do
    assertEquals (toList l) []
    assertEquals v Nothing
    assertEquals (toList r) [1, 2, 4, 5]
    pure ()


testSplit : IO ()
testSplit = do
  testSplit1
  testSplit2
  testSplit3
  testSplit4


testMerge : IO ()
testMerge =
  let tree1 = toTree [1, 3, 5] in
  let tree2 = toTree [2, 3, 4] in
  let tree = merge tree1 tree2 in
  do
    assertEquals (toList tree) [1, 2, 3, 4, 5]
    pure ()

-- testFunctor : IO ()
-- testFunctor =
--   let tree = toTree [1, 2, 3, 4, 5] in
--   let tree = map (\n = if n `mod` 2 == 1
--                        then n
--                        else n / 2) in
--   do
--     assertEquals (toList tree) [1, 2, 3, 5]
--     pure ()

export
test : IO ()
test = do
  testInsertMember
  testToTreeMember
  testToTreeToList
  testSplit
  testMerge
