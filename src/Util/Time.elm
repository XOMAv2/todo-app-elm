module Util.Time exposing (..)

import Time exposing (..)
import Util as U


monthToNumber : Month -> Int
monthToNumber month =
    case month of
        Jan ->
            1

        Feb ->
            2

        Mar ->
            3

        Apr ->
            4

        May ->
            5

        Jun ->
            6

        Jul ->
            7

        Aug ->
            8

        Sep ->
            9

        Oct ->
            10

        Nov ->
            11

        Dec ->
            12


isLeapYear : Int -> Bool
isLeapYear year =
    (U.isZero (modBy 4 year) && U.isNotZero (modBy 100 year)) || U.isZero (modBy 400 year)


daysInMonth : Int -> Month -> Int
daysInMonth year month =
    case month of
        Jan ->
            31

        Feb ->
            if isLeapYear year then
                29

            else
                28

        Mar ->
            31

        Apr ->
            30

        May ->
            31

        Jun ->
            30

        Jul ->
            31

        Aug ->
            31

        Sep ->
            30

        Oct ->
            31

        Nov ->
            30

        Dec ->
            31



-- COMPORATORS


type alias Comparator a =
    Zone -> Posix -> Posix -> a


makeComparator : (Zone -> Posix -> Int) -> Comparator Int
makeComparator segmentExtractor =
    \zone x y -> segmentExtractor zone x - segmentExtractor zone y


yearCompare : Comparator Int
yearCompare =
    makeComparator toYear


monthCompare : Comparator Int
monthCompare =
    makeComparator (\zone v -> monthToNumber (toMonth zone v))


dayCompare : Comparator Int
dayCompare =
    makeComparator toDay


hourCompare : Comparator Int
hourCompare =
    makeComparator toHour


minuteCompare : Comparator Int
minuteCompare =
    makeComparator toMinute


secondCompare : Comparator Int
secondCompare =
    makeComparator toSecond


millisCompareZone : Comparator Int
millisCompareZone _ =
    millisCompare


millisCompare : Posix -> Posix -> Int
millisCompare x y =
    modBy 1000 (posixToMillis x) - modBy 1000 (posixToMillis y)


compare : Posix -> Posix -> Int
compare x y =
    posixToMillis x - posixToMillis y



-- DELTAS


type alias TimeDelta =
    { millis : Int
    , second : Int
    , minute : Int
    , hour : Int
    , day : Int
    , month : Int
    , year : Int
    }


map : (Int -> Int) -> TimeDelta -> TimeDelta
map f record =
    { millis = f record.millis
    , second = f record.second
    , minute = f record.minute
    , hour = f record.hour
    , day = f record.day
    , month = f record.month
    , year = f record.year
    }


timeDelta : Comparator TimeDelta
timeDelta zone x y =
    let
        ( from, to, sign ) =
            if U.isNeg (compare x y) then
                ( x, y, -1 )

            else
                ( y, x, 1 )

        fix v n =
            (v + n) |> remainderBy n
    in
    { millis = millisCompare to from
    , second = secondCompare zone to from
    , minute = minuteCompare zone to from
    , hour = hourCompare zone to from
    , day = dayCompare zone to from
    , month = monthCompare zone to from
    , year = yearCompare zone to from
    }
        |> U.update (.millis >> U.isNeg)
            (\v -> { v | second = U.dec v.second, millis = fix v.millis 1000 })
        |> U.update (.second >> U.isNeg)
            (\v -> { v | minute = U.dec v.minute, second = fix v.second 60 })
        |> U.update (.minute >> U.isNeg)
            (\v -> { v | hour = U.dec v.hour, minute = fix v.minute 60 })
        |> U.update (.hour >> U.isNeg)
            (\v -> { v | day = U.dec v.day, hour = fix v.hour 24 })
        |> U.update (.day >> U.isNeg)
            (\v ->
                { v
                    | month = U.dec v.month
                    , day = fix v.day (daysInMonth (toYear zone to) (toMonth zone to))
                }
            )
        |> U.update (.month >> U.isNeg)
            (\v -> { v | year = U.dec v.year, month = fix v.month 12 })
        |> U.update (always (U.isNeg sign))
            (\v -> map ((*) sign) v)
