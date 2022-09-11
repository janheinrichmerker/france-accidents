module Partition exposing (Filter, Partitioner, Partitioners, Partitions, equalityPartitioner, maybeEqualityPartitioner, partitionTree)

import Tree exposing (Tree)


type alias Filter a comparable =
    ( a -> Bool, comparable )


type alias Partitioner a comparable =
    List (Filter a comparable)


type alias Partitioners a comparable =
    List (Partitioner a comparable)


type alias Partition a comparable =
    ( List a, comparable )


type alias Partitions a comparable =
    List (Partition a comparable)


type alias PartitionTree a comparable =
    Tree (Partition a (List comparable))


equalityFilter : (a -> v) -> v -> comparable -> Filter a comparable
equalityFilter mapper reference name =
    ( \element -> mapper element == reference, name )


maybeEqualityFilter : (a -> Maybe v) -> v -> comparable -> Filter a comparable
maybeEqualityFilter mapper reference name =
    ( \element ->
        case mapper element of
            Nothing ->
                False

            Just x ->
                x == reference
    , name
    )


equalityPartitioner : (a -> v) -> List ( v, comparable ) -> Partitioner a comparable
equalityPartitioner mapper references =
    List.map (\( ref, name ) -> equalityFilter mapper ref name) references


maybeEqualityPartitioner : (a -> Maybe v) -> List ( v, comparable ) -> Partitioner a comparable
maybeEqualityPartitioner mapper references =
    List.map (\( ref, name ) -> maybeEqualityFilter mapper ref name) references


partition : List a -> Partitioner a comparable -> Partitions a comparable
partition elements partitioner =
    List.map (\( filter, name ) -> ( List.filter filter elements, name )) partitioner


partitionTree : Partitioners a comparable -> List a -> PartitionTree a comparable
partitionTree partitioners =
    partitionTreeHelp partitioners []


partitionTreeHelp : Partitioners a comparable -> List comparable -> List a -> PartitionTree a comparable
partitionTreeHelp partitioners labels elements =
    case partitioners of
        [] ->
            -- No partitioner left, so we cannot partition the elements further.
            Tree.singleton ( elements, labels )

        firstPartitioner :: remainingPartitioners ->
            -- Partition the elements across by the first partitioner
            -- and then build trees for each partition using the remaining partitioners.
            let
                firstPartitions : Partitions a comparable
                firstPartitions =
                    partition elements firstPartitioner

                mapPartition : Partition a comparable -> PartitionTree a comparable
                mapPartition ( singePartition, singleLabel ) =
                    partitionTreeHelp remainingPartitioners (labels ++ [ singleLabel ]) singePartition

                mapPartitions : Partitions a comparable -> List (PartitionTree a comparable)
                mapPartitions partitions =
                    partitions |> List.map mapPartition
            in
            Tree.tree
                ( elements, [] )
                (mapPartitions firstPartitions)
