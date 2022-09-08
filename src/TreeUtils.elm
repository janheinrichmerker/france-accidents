module TreeUtils exposing (..)

import Tree exposing (Tree)
import TreeDiagram


toTreeLayout : Tree a -> TreeDiagram.Tree a
toTreeLayout tree =
    Tree.restructure identity TreeDiagram.node tree
