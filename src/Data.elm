module Data exposing (..)

import Csv.Decode exposing (Decoder, Error, FieldNames(..), andThen, decodeCsv, fail, field, float, int, into, map, pipeline, string, succeed)
import Model exposing (AccidentId, Address, Characteristic, Collision(..), Coordinates, Departement, GeoOriginator(..), Intersection(..), Lighting(..), Localisation(..), Muncipality, Weather(..))
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
                fail ("Invalid hour format '" ++ String.fromInt hour ++ "'")

            else if minute < 0 || minute > 59 then
                fail ("Invalid minute format '" ++ String.fromInt minute ++ "'")

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
                    fail ("Unknown lighting '" ++ String.fromInt lighting ++ "'.")
        )
        (field "lum" int)


decodeCharacteristicDepartement : Decoder Departement
decodeCharacteristicDepartement =
    field "dep" int


decodeCharacteristicMuncipality : Decoder Muncipality
decodeCharacteristicMuncipality =
    field "com" int


decodeCharacteristicLocalisation : Decoder Localisation
decodeCharacteristicLocalisation =
    andThen
        (\localisation ->
            case localisation of
                1 ->
                    succeed LocalisationOutOfAgglomeration

                2 ->
                    succeed LocalisationInBuiltUpAreas

                _ ->
                    fail ("Unknown localisation '" ++ String.fromInt localisation ++ "'.")
        )
        (field "agg" int)


decodeCharacteristicIntersection : Decoder Intersection
decodeCharacteristicIntersection =
    andThen
        (\intersection ->
            case intersection of
                1 ->
                    succeed IntersectionOutOfIntersection

                2 ->
                    succeed IntersectionX

                3 ->
                    succeed IntersectionT

                4 ->
                    succeed IntersectionY

                5 ->
                    succeed IntersectionMoreThan4Branches

                6 ->
                    succeed IntersectionGiratory

                7 ->
                    succeed IntersectionPlace

                8 ->
                    succeed IntersectionLevelCrossing

                9 ->
                    succeed IntersectionOther

                _ ->
                    fail ("Unknown intersection '" ++ String.fromInt intersection ++ "'.")
        )
        (field "int" int)


decodeCharacteristicWeather : Decoder Weather
decodeCharacteristicWeather =
    andThen
        (\weather ->
            case weather of
                1 ->
                    succeed WeatherNormal

                2 ->
                    succeed WeatherLightRain

                3 ->
                    succeed WeatherHeavyRain

                4 ->
                    succeed WeatherSnowHail

                5 ->
                    succeed WeatherFogSmoke

                6 ->
                    succeed WeatherStrongWindStorm

                7 ->
                    succeed WeatherDazzling

                8 ->
                    succeed WeatherCloudy

                9 ->
                    succeed WeatherOther

                _ ->
                    fail ("Unknown weather '" ++ String.fromInt weather ++ "'.")
        )
        (field "atm" int)


decodeCharacteristicCollision : Decoder Collision
decodeCharacteristicCollision =
    andThen
        (\collision ->
            case collision of
                1 ->
                    succeed CollisionTwoVehiclesFrontal

                2 ->
                    succeed CollisionTwoVehiclesRear

                3 ->
                    succeed CollisionTwoVehiclesSide

                4 ->
                    succeed CollisionThreeOrMoreVehiclesChain

                5 ->
                    succeed CollisionThreeOrMoreVehiclesMultipleCollisions

                6 ->
                    succeed CollisionOther

                7 ->
                    succeed CollisionNone

                _ ->
                    fail ("Unknown collision '" ++ String.fromInt collision ++ "'.")
        )
        (field "col" int)


decodeCharacteristicAddress : Decoder Address
decodeCharacteristicAddress =
    field "adr" string


decodeCharacteristicGeoOriginator : Decoder GeoOriginator
decodeCharacteristicGeoOriginator =
    andThen
        (\geoOriginator ->
            case geoOriginator of
                "M" ->
                    succeed GeoOriginatorMetropole

                "A" ->
                    succeed GeoOriginatorAntilles

                "G" ->
                    succeed GeoOriginatorGuyane

                "R" ->
                    succeed GeoOriginatorReunion

                "Y" ->
                    succeed GeoOriginatorMayotte

                _ ->
                    fail ("Unknown geo originator '" ++ geoOriginator ++ "'.")
        )
        (field "gps" string)


decodeCharacteristicCoordinates : Decoder Coordinates
decodeCharacteristicCoordinates =
    into Coordinates
        |> pipeline (field "lat" float)
        |> pipeline (field "lat" float)
