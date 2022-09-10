module TreeUtils exposing (toTreeDiagram)

import Tree exposing (Tree)
import TreeDiagram


toTreeDiagram : Tree a -> TreeDiagram.Tree a
toTreeDiagram tree =
    Tree.restructure identity TreeDiagram.node tree
