module Data exposing (..)

import Csv.Decode exposing (Decoder, Error, FieldNames(..), andThen, decodeCsv, fail, field, int, into, map, pipeline, string, succeed)
import Model exposing (AccidentId, Address, Characteristic, Collision, Coordinates, Departement, GeoOriginator, Intersection, Lighting(..), Localisation, Muncipality, Weather)
import Time exposing (Posix)
import Time.DateTime exposing (DateTime, dateTime, minute)
import Time.TimeZones exposing (europe_paris)
import Time.ZonedDateTime exposing (ZonedDateTime, fromDateTime, toPosix, zero)
import Tuple exposing (first, pair, second)


parseCharacteristics : String -> Result Error (List ( AccidentId, Characteristic ))
parseCharacteristics lines =
    decodeCsv FieldNamesFromFirstRow decodeAccidentCharacteristic lines


decodeAccidentCharacteristic : Decoder ( AccidentId, Characteristic )
decodeAccidentCharacteristic =
    into pair
        |> pipeline (field "Num_Acc" string)
        |> pipeline decodeCharacteristic


decodeCharacteristic : Decoder Characteristic
decodeCharacteristic =
    into Characteristic
        |> pipeline decodeCharacteristicPosix
        |> pipeline decodeCharacteristicLighting
        |> pipeline decodeCharacteristicDepartement
        |> pipeline decodeCharacteristicMuncipality
        |> pipeline decodeCharacteristicLocalisation
        |> pipeline decodeCharacteristicIntersection
        |> pipeline decodeCharacteristicWeather
        |> pipeline decodeCharacteristicCollision
        |> pipeline decodeCharacteristicAddress
        |> pipeline decodeCharacteristicGeoOriginator
        |> pipeline decodeCharacteristicCoordinates


decodeCharacteristicPosix : Decoder Posix
decodeCharacteristicPosix =
    map toPosix decodeCharacteristicZonedDateTime


decodeCharacteristicZonedDateTime : Decoder ZonedDateTime
decodeCharacteristicZonedDateTime =
    map (fromDateTime europe_paris) decodeCharacteristicDateTime


decodeCharacteristicDateTime : Decoder DateTime
decodeCharacteristicDateTime =
    into
        (\year month day hour minute ->
            dateTime
                { zero
                    | year = year
                    , month = month
                    , day = day
                    , hour = hour
                    , minute = minute
                }
        )
        |> pipeline decodeCharacteristicYear
        |> pipeline decodeCharacteristicMonth
        |> pipeline decodeCharacteristicDay
        |> pipeline decodeCharacteristicHour
        |> pipeline decodeCharacteristicMinute


decodeCharacteristicYear : Decoder Int
decodeCharacteristicYear =
    map
        (\twoDigitYear -> 2000 + twoDigitYear)
        (field "an" int)


decodeCharacteristicMonth : Decoder Int
decodeCharacteristicMonth =
    field "mois" int


decodeCharacteristicDay : Decoder Int
decodeCharacteristicDay =
    field "jour" int


decodeCharacteristicHour : Decoder Int
decodeCharacteristicHour =
    map first decodeCharacteristicHourMinute


decodeCharacteristicMinute : Decoder Int
decodeCharacteristicMinute =
    map second decodeCharacteristicHourMinute


decodeCharacteristicHourMinute : Decoder ( Int, Int )
decodeCharacteristicHourMinute =
    andThen
        (\hourMinute ->
            let
                hour : Int
                hour =
                    hourMinute // (10 ^ 2)

                minute : Int
                minute =
                    hourMinute - (hour * (10 ^ 2))
            in
            if hour < 0 || hour > 24 then
                fail "Invalid hour format"

            else if minute < 0 || minute > 59 then
                fail "Invalid hour format"

            else
                succeed ( hour, minute )
        )
        (field "hrmn" int)


decodeCharacteristicLighting : Decoder Lighting
decodeCharacteristicLighting =
    andThen
        (\lighting ->
            case lighting of
                1 ->
                    succeed LightingFullDay

                2 ->
                    succeed LightingTwilightDawn

                3 ->
                    succeed LightingNightWithoutPublicLighting

                4 ->
                    succeed LightingNightWithPublicLightingNotLit

                5 ->
                    succeed LightingNightWithPublicLighting

                _ ->
                    fail "Unknown lighting."
        )
        (field "lum" int)


decodeCharacteristicDepartement : Decoder Departement
decodeCharacteristicDepartement =
    fail "not implemented"


decodeCharacteristicMuncipality : Decoder Muncipality
decodeCharacteristicMuncipality =
    fail "not implemented"


decodeCharacteristicLocalisation : Decoder Localisation
decodeCharacteristicLocalisation =
    fail "not implemented"


decodeCharacteristicIntersection : Decoder Intersection
decodeCharacteristicIntersection =
    fail "not implemented"


decodeCharacteristicWeather : Decoder Weather
decodeCharacteristicWeather =
    fail "not implemented"


decodeCharacteristicCollision : Decoder Collision
decodeCharacteristicCollision =
    fail "not implemented"


decodeCharacteristicAddress : Decoder Address
decodeCharacteristicAddress =
    fail "not implemented"


decodeCharacteristicGeoOriginator : Decoder GeoOriginator
decodeCharacteristicGeoOriginator =
    fail "not implemented"


decodeCharacteristicCoordinates : Decoder Coordinates
decodeCharacteristicCoordinates =
    fail "not implemented"
