module TimeUtils exposing (..)

import Time exposing (Posix)
import Time.Date exposing (Weekday(..))
import Time.DateTime exposing (DateTime, addDays, fromPosix, setDay, setHour, setMillisecond, setMinute, setSecond, setYear, toPosix, weekday)


setWeekday : Weekday -> DateTime -> DateTime
setWeekday newDay date =
    if weekday date == newDay then
        date

    else
        date |> addDays -1 |> setWeekday newDay


retainMonth : Posix -> Posix
retainMonth time =
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


retainWeek : Posix -> Posix
retainWeek time =
    let
        date : DateTime
        date =
            fromPosix time

        floorDate : DateTime
        floorDate =
            date
                |> setWeekday Mon
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
