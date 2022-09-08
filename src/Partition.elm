module Partition exposing (..)

import Tree exposing (Tree)


type alias Filter a =
    a -> Bool


type alias Partitioner a =
    List (Filter a)


type alias Partitioners a =
    List (Partitioner a)


type alias Partitions a =
    List (List a)


equalityFilter : (a -> v) -> v -> Filter a
equalityFilter mapper reference element =
    mapper element == reference


equalityPartitioner : (a -> v) -> List v -> Partitioner a
equalityPartitioner mapper references =
    List.map (equalityFilter mapper) references


partition : List a -> Partitioner a -> Partitions a
partition elements partitioner =
    List.map (\filter -> List.filter filter elements) partitioner


partitionTree : List a -> Partitioners a -> Tree (List a)
partitionTree elements partitioners =
    case partitioners of
        [] ->
            -- No partitioner left, so we cannot partition the elements further.
            Tree.singleton elements

        firstPartitioner :: remainingPartitioners ->
            -- Partition the elements across by the first partitioner
            -- and then build trees for each partition using the remaining partitioners.
            let
                firstPartitions : Partitions a
                firstPartitions =
                    partition elements firstPartitioner
            in
            Tree.tree
                elements
                (List.map
                    (\singePartition -> partitionTree singePartition remainingPartitioners)
                    firstPartitions
                )
