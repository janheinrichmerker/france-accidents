module Utils exposing (..)

import Dict exposing (Dict)
import List


reverseTuple : ( a, b ) -> ( b, a )
reverseTuple ( a, b ) =
    ( b, a )


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
