module Tests.Util.Time exposing (..)

import Expect exposing (Expectation)
import Iso8601
import Test exposing (..)
import Time
import Util.Time exposing (..)


iso : Comparator a -> Time.Zone -> String -> String -> Maybe a
iso f zone xIso yIso =
    case ( Iso8601.toTime xIso, Iso8601.toTime yIso ) of
        ( Ok x, Ok y ) ->
            Just (f zone x y)

        _ ->
            Nothing


timeEqual : Comparator a -> List ( String, String, a ) -> Expectation
timeEqual comparator args =
    Expect.equalLists
        (List.map (\( _, _, expected ) -> Just expected)
            args
        )
        (List.map (\( x, y, _ ) -> iso comparator Time.utc x y)
            args
        )


xCompareTests : Test
xCompareTests =
    describe "Tests for family of compare functions."
        [ describe "Milliseconds comparator."
            [ test "Comparison only within the specified segment." <|
                \_ ->
                    timeEqual millisCompareZone
                        [ ( "2023-01-01T00:00:00.111", "2023-01-01T00:00:00.222", -111 )
                        , ( "2023-01-01T00:00:00.456", "2023-01-01T00:00:00.123", 333 )
                        , ( "2023-01-01T00:00:00.777", "2023-01-01T00:00:00.777", 0 )
                        , ( "2023-01-01T00:00:00.000", "2023-01-01T00:00:00.999", -999 )
                        , ( "2023-01-01T00:00:00.999", "2023-01-01T00:00:00.000", 999 )
                        ]
            , test "Values in other segments do not affect the result." <|
                \_ ->
                    timeEqual millisCompareZone
                        [ ( "2023-07-01T09:00:10.111", "2024-01-14T06:20:00.222", -111 ) ]
            , test "Values before and after 1970." <|
                \_ ->
                    timeEqual millisCompareZone
                        [ ( "1001-01-01T00:00:00.002", "2002-01-01T00:00:00.001", 1 )
                        , ( "1001-01-01T00:00:00.001", "2002-01-01T00:00:00.002", -1 )
                        , ( "1001-01-01T00:00:00.001", "2002-01-01T00:00:00.001", 0 )
                        ]
            ]
        , describe "Seconds comparator."
            [ test "Comparison only within the specified segment." <|
                \_ ->
                    timeEqual secondCompare
                        [ ( "2023-01-01T00:00:11.000", "2023-01-01T00:00:33.000", -22 )
                        , ( "2023-01-01T00:00:45.000", "2023-01-01T00:00:10.000", 35 )
                        , ( "2023-01-01T00:00:30.000", "2023-01-01T00:00:30.000", 0 )
                        , ( "2023-01-01T00:00:00.000", "2023-01-01T00:00:59.000", -59 )
                        , ( "2023-01-01T00:00:59.000", "2023-01-01T00:00:00.000", 59 )
                        ]
            , test "Values in other segments do not affect the result." <|
                \_ ->
                    timeEqual secondCompare
                        [ ( "2023-07-01T09:00:10.111", "2024-01-14T06:20:00.222", 10 ) ]
            , test "Values before and after 1970." <|
                \_ ->
                    timeEqual secondCompare
                        [ ( "1001-01-01T00:00:01.000", "2002-01-01T00:00:00.000", 1 )
                        , ( "1001-01-01T00:00:00.000", "2002-01-01T00:00:01.000", -1 )
                        , ( "1001-01-01T00:00:01.000", "2002-01-01T00:00:01.000", 0 )
                        ]
            ]
        , describe "Minutes comparator."
            [ test "Comparison only within the specified segment." <|
                \_ ->
                    timeEqual minuteCompare
                        [ ( "2023-01-01T00:11:00.000", "2023-01-01T00:33:00.000", -22 )
                        , ( "2023-01-01T00:45:00.000", "2023-01-01T00:10:00.000", 35 )
                        , ( "2023-01-01T00:30:00.000", "2023-01-01T00:30:00.000", 0 )
                        , ( "2023-01-01T00:00:00.000", "2023-01-01T00:59:00.000", -59 )
                        , ( "2023-01-01T00:59:00.000", "2023-01-01T00:00:00.000", 59 )
                        ]
            , test "Values in other segments do not affect the result." <|
                \_ ->
                    timeEqual minuteCompare
                        [ ( "2023-07-01T09:00:10.111", "2024-01-14T06:20:00.222", -20 ) ]
            , test "Values before and after 1970." <|
                \_ ->
                    timeEqual minuteCompare
                        [ ( "1001-01-01T00:01:00.000", "2002-01-01T00:00:00.000", 1 )
                        , ( "1001-01-01T00:00:00.000", "2002-01-01T00:01:00.000", -1 )
                        , ( "1001-01-01T00:01:00.000", "2002-01-01T00:01:00.000", 0 )
                        ]
            ]
        , describe "Hour comparator."
            [ test "Comparison only within the specified segment." <|
                \_ ->
                    timeEqual hourCompare
                        [ ( "2023-01-01T11:00:00.000", "2023-01-01T22:00:00.000", -11 )
                        , ( "2023-01-01T19:00:00.000", "2023-01-01T06:00:00.000", 13 )
                        , ( "2023-01-01T04:00:00.000", "2023-01-01T04:00:00.000", 0 )
                        , ( "2023-01-01T00:00:00.000", "2023-01-01T23:00:00.000", -23 )
                        , ( "2023-01-01T23:00:00.000", "2023-01-01T00:00:00.000", 23 )
                        ]
            , test "Values in other segments do not affect the result." <|
                \_ ->
                    timeEqual hourCompare
                        [ ( "2023-07-01T09:00:10.111", "2024-01-14T06:20:00.222", 3 ) ]
            , test "Values before and after 1970." <|
                \_ ->
                    timeEqual hourCompare
                        [ ( "1001-01-01T01:00:00.000", "2002-01-01T00:00:00.000", 1 )
                        , ( "1001-01-01T00:00:00.000", "2002-01-01T01:00:00.000", -1 )
                        , ( "1001-01-01T01:00:00.000", "2002-01-01T01:00:00.000", 0 )
                        ]
            ]
        , describe "Day comparator."
            [ test "Comparison only within the specified segment." <|
                \_ ->
                    timeEqual dayCompare
                        [ ( "2023-01-11", "2023-01-26", -15 )
                        , ( "2023-01-17", "2023-01-03", 14 )
                        , ( "2023-01-23", "2023-01-23", 0 )
                        , ( "2023-01-01", "2023-01-31", -30 )
                        , ( "2023-01-31", "2023-01-01", 30 )
                        ]
            , test "Values in other segments do not affect the result." <|
                \_ ->
                    timeEqual dayCompare
                        [ ( "2023-07-01T09:00:10.111", "2024-01-14T06:20:00.222", -13 ) ]
            , test "Values before and after 1970." <|
                \_ ->
                    timeEqual dayCompare
                        [ ( "1001-01-02", "2002-01-01", 1 )
                        , ( "1001-01-01", "2002-01-02", -1 )
                        , ( "1001-01-02", "2002-01-02", 0 )
                        ]
            ]
        , describe "Month comparator."
            [ test "Comparison only within the specified segment." <|
                \_ ->
                    timeEqual monthCompare
                        [ ( "2023-09-01", "2023-11-01", -2 )
                        , ( "2023-12-01", "2023-04-01", 8 )
                        , ( "2023-09-01", "2023-09-01", 0 )
                        , ( "2023-01-01", "2023-12-01", -11 )
                        , ( "2023-12-01", "2023-01-01", 11 )
                        ]
            , test "Values in other segments do not affect the result." <|
                \_ ->
                    timeEqual monthCompare
                        [ ( "2023-07-01T09:00:10.111", "2024-01-14T06:20:00.222", 6 ) ]
            , test "Values before and after 1970." <|
                \_ ->
                    timeEqual monthCompare
                        [ ( "1001-02-01", "2002-01-01", 1 )
                        , ( "1001-01-01", "2002-02-01", -1 )
                        , ( "1001-02-01", "2002-02-01", 0 )
                        ]
            ]
        , describe "Year comparator."
            [ test "Comparison only within the specified segment." <|
                \_ ->
                    timeEqual yearCompare
                        [ ( "0012-01-01", "3015-01-01", -3003 )
                        , ( "8701-01-01", "1023-01-01", 7678 )
                        , ( "2023-01-01", "2023-01-01", 0 )
                        , ( "0001-01-01", "9999-01-01", -9998 )
                        , ( "9999-01-01", "0001-01-01", 9998 )
                        ]
            , test "Values in other segments do not affect the result." <|
                \_ ->
                    timeEqual yearCompare
                        [ ( "2023-07-01T09:00:10.111", "2024-01-14T06:20:00.222", -1 ) ]
            , test "Values before and after 1970." <|
                \_ ->
                    timeEqual yearCompare
                        [ ( "1001-01-01", "2002-01-01", -1001 )
                        , ( "2002-01-01", "1001-01-01", 1001 )
                        , ( "1970-01-01", "1970-01-01", 0 )
                        ]
            ]
        ]


timeDeltaTests : Test
timeDeltaTests =
    describe "Testing the calculation of the time delta within a time interval."
        [ test "Simple cases." <|
            \_ ->
                let
                    tests =
                        [ ( "1011-11-11T11:11:11.110"
                          , "1010-10-10T10:10:10.100"
                          , { millis = 10
                            , second = 1
                            , minute = 1
                            , hour = 1
                            , day = 1
                            , month = 1
                            , year = 1
                            }
                          )
                        , ( "1012-02-12T11:11:11.110"
                          , "1010-03-12T11:11:11.110"
                          , { millis = 0
                            , second = 0
                            , minute = 0
                            , hour = 0
                            , day = 0
                            , month = 11
                            , year = 1
                            }
                          )
                        ]
                in
                timeEqual timeDelta
                    (tests ++ List.map (\( x, y, e ) -> ( y, x, map ((*) -1) e )) tests)
        , test "Segment transfer." <|
            \_ ->
                let
                    tests =
                        [ ( "1011-11-11T11:11:11.110"
                          , "1010-12-12T12:12:12.120"
                          , { millis = 990
                            , second = 58
                            , minute = 58
                            , hour = 22
                            , day = 28
                            , month = 10
                            , year = 0
                            }
                          )
                        , ( "1002-01-01T00:00:00.001"
                          , "1001-01-01T00:00:00.002"
                          , { millis = 999
                            , second = 59
                            , minute = 59
                            , hour = 23
                            , day = 30
                            , month = 11
                            , year = 0
                            }
                          )
                        , ( "1002-01-01T00:00:00.001"
                          , "1001-01-01T00:00:00.002"
                          , { millis = 999
                            , second = 59
                            , minute = 59
                            , hour = 23
                            , day = 30
                            , month = 11
                            , year = 0
                            }
                          )
                        ]
                in
                timeEqual timeDelta
                    (tests ++ List.map (\( x, y, e ) -> ( y, x, map ((*) -1) e )) tests)
        , test "Values before and after 1970." <|
            \_ ->
                let
                    tests =
                        [ ( "2002-01-01T00:00:00.001"
                          , "1001-01-01T00:00:00.002"
                          , { millis = 999
                            , second = 59
                            , minute = 59
                            , hour = 23
                            , day = 30
                            , month = 11
                            , year = 1000
                            }
                          )
                        ]
                in
                timeEqual timeDelta
                    (tests ++ List.map (\( x, y, e ) -> ( y, x, map ((*) -1) e )) tests)
        , describe "Time zone consideration."
            [ test "Europe/London" <|
                -- In Elm there is no way to define a time zone by its name,
                -- so I use the offset in the date.
                \_ ->
                    timeEqual timeDelta
                        [ ( "2023-03-24T00:00:00.000+01:00"
                          , "2023-03-23T00:00:00.000"
                          , { millis = 0
                            , second = 0
                            , minute = 0
                            , hour = 23
                            , day = 0
                            , month = 0
                            , year = 0
                            }
                          )
                        ]
            ]
        ]
