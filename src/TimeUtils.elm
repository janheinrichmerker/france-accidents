module TimeUtils exposing (Quarter, removeYear, retainMonth, retainQuarter, retainWeek, retainYear)

import Time exposing (Posix)
import Time.Date exposing (Weekday(..))
import Time.DateTime exposing (DateTime, addDays, fromPosix, month, setDay, setHour, setMillisecond, setMinute, setMonth, setSecond, setYear, toPosix, weekday)


type Quarter
    = FirstQuarter
    | SecondQuarter
    | ThirdQuarter
    | FourthQuarter


quarter : DateTime -> Quarter
quarter date =
    let
        mon : Int
        mon =
            month date
    in
    if 1 <= mon && mon <= 3 then
        FirstQuarter

    else if 4 <= mon && mon <= 6 then
        SecondQuarter

    else if 7 <= mon && mon <= 9 then
        ThirdQuarter

    else
        FourthQuarter


setQuarterMonth : Int -> DateTime -> DateTime
setQuarterMonth quarterMonth date =
    let
        offset : number
        offset =
            case quarter date of
                FirstQuarter ->
                    0

                SecondQuarter ->
                    3

                ThirdQuarter ->
                    6

                FourthQuarter ->
                    9
    in
    setMonth (offset + quarterMonth) date


setWeekday : Weekday -> DateTime -> DateTime
setWeekday newDay date =
    if weekday date == newDay then
        date

    else
        date |> addDays -1 |> setWeekday newDay


retainYear : Posix -> Posix
retainYear time =
    let
        date : DateTime
        date =
            fromPosix time

        floorDate : DateTime
        floorDate =
            date
                |> setMonth 1
                |> setDay 0
                |> setHour 0
                |> setMinute 0
                |> setSecond 0
                |> setMillisecond 0
    in
    toPosix floorDate


retainQuarter : Posix -> Posix
retainQuarter time =
    let
        date : DateTime
        date =
            fromPosix time

        floorDate : DateTime
        floorDate =
            date
                |> setQuarterMonth 1
                |> setDay 0
                |> setHour 0
                |> setMinute 0
                |> setSecond 0
                |> setMillisecond 0
    in
    toPosix floorDate


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
