module Utils exposing (hashString, isBetween, mapConsecutive, reverseTuple, sortTuple, toBucketDict, tupleMax, tupleMean)

import Char exposing (toCode)
import Dict exposing (Dict)
import List


reverseTuple : ( a, b ) -> ( b, a )
reverseTuple ( a, b ) =
    ( b, a )


mapConsecutive : (a -> a -> b) -> List a -> Maybe (List b)
mapConsecutive mapper list =
    Maybe.map (\l2 -> List.map2 mapper list l2) <| List.tail list


appendToOrCreateList : a -> Maybe (List a) -> List a
appendToOrCreateList newElement oldList =
    case oldList of
        Nothing ->
            [ newElement ]

        Just list ->
            List.append list [ newElement ]


maybeAppendToOrCreateList : a -> Maybe (List a) -> Maybe (List a)
maybeAppendToOrCreateList newElement oldList =
    Just (appendToOrCreateList newElement oldList)


insertIntoBucket : ( comparable, a ) -> BucketDict comparable a -> BucketDict comparable a
insertIntoBucket ( key, accident ) dict =
    Dict.update key (maybeAppendToOrCreateList accident) dict


type alias BucketDict comparable a =
    Dict comparable (List a)


toBucketDict : List ( comparable, a ) -> Dict comparable (List a)
toBucketDict =
    List.foldl
        insertIntoBucket
        Dict.empty


tupleMean : ( Float, Float ) -> Float
tupleMean t =
    Tuple.first t + (Tuple.second t - Tuple.first t) / 2


tupleMin : ( comparable, comparable ) -> comparable
tupleMin ( a, b ) =
    min a b


tupleMax : ( comparable, comparable ) -> comparable
tupleMax ( a, b ) =
    max a b


sortTuple : ( Float, Float ) -> ( Float, Float )
sortTuple t =
    ( tupleMin t, tupleMax t )


{-| Polynomial rolling hash function for strings,
inspired by <https://cp-algorithms.com/string/string-hashing.html#calculation-of-the-hash-of-a-string>
-}
hashString : String -> Int
hashString string =
    let
        p : Int
        p =
            31

        m : Int
        m =
            round 1.0e9 + 9
    in
    String.foldl
        (\char ( hash, pPow ) ->
            let
                code : Int
                code =
                    toCode char
            in
            ( (hash + code * pPow) |> modBy m
            , (pPow * p) |> modBy m
            )
        )
        ( 0, 1 )
        string
        |> Tuple.first


isBetween : number -> number -> number -> Bool
isBetween bound1 bound2 value =
    let
        boundMin : number
        boundMin =
            min bound1 bound2

        boundMax : number
        boundMax =
            max bound1 bound2
    in
    boundMin <= value && value <= boundMax
