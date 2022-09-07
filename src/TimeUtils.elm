module TimeUtils exposing (..)

import Time exposing (Posix)
import Time.DateTime exposing (DateTime, fromPosix, setDay, setHour, setMillisecond, setMinute, setSecond, setYear, toPosix)


retainDay : Posix -> Posix
retainDay time =
    let
        date : DateTime
        date =
            fromPosix time

        floorDate : DateTime
        floorDate =
            date
                |> setHour 0
                |> setMinute 0
                |> setSecond 0
                |> setMillisecond 0
    in
    toPosix floorDate


retainWeek : Posix -> Posix
retainWeek time =
    let
        date : DateTime
        date =
            fromPosix time

        floorDate : DateTime
        floorDate =
            date
                |> setDay 0
                |> setHour 0
                |> setMinute 0
                |> setSecond 0
                |> setMillisecond 0
    in
    toPosix floorDate


removeYear : Posix -> Posix
removeYear time =
    let
        date : DateTime
        date =
            fromPosix time

        floorDate : DateTime
        floorDate =
            date
                |> setYear 0
    in
    toPosix floorDate
