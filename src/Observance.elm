module Observance exposing (Table, decoder, empty, get)

import Date exposing (Date)
import Dict exposing (Dict)
import I18Next exposing (Translations)
import Json.Decode as D


type Table
    = Table (Dict Int (List Meta))


type alias Meta =
    { key : String
    , sinceYear : Maybe Int
    }


type alias JsonEntry =
    { meta : Meta
    , key : Key
    }


type Key
    = Date DateOfYear
    | Weekday WeekdayOfMonth


type alias DateOfYear =
    { month : Int
    , -- We are using JS's terminology here (`Date.prototype.getDate()`)
      -- because we are going to get this through flags.
      date : Int
    }


type alias WeekdayOfMonth =
    { month : Int
    , week : Int
    , day : Int
    }


empty : Table
empty =
    Table Dict.empty


decoder : D.Decoder Table
decoder =
    D.list entryDecoder
        |> D.map
            (Table
                << List.foldl
                    (\entry ->
                        Dict.update (keyToInt entry.key)
                            (\list -> Just <| entry.meta :: Maybe.withDefault [] list)
                    )
                    Dict.empty
            )


entryDecoder : D.Decoder JsonEntry
entryDecoder =
    D.map2 JsonEntry metaDecoder keyDecoder


metaDecoder : D.Decoder Meta
metaDecoder =
    D.map2 Meta (D.field "key" D.string) (D.maybe (D.field "sinceYear" D.int))


keyDecoder : D.Decoder Key
keyDecoder =
    D.oneOf [ dateDecoder |> D.map Date, weekdayDecoder |> D.map Weekday ]


dateDecoder : D.Decoder DateOfYear
dateDecoder =
    D.map2 DateOfYear (D.field "month" D.int) (D.field "date" D.int)


weekdayDecoder : D.Decoder WeekdayOfMonth
weekdayDecoder =
    D.map3 WeekdayOfMonth (D.field "month" D.int) (D.field "week" D.int) (D.field "day" D.int)


get : Translations -> Date -> Table -> List String
get translations date (Table dict) =
    let
        year =
            Date.year date

        xs =
            dict |> Dict.get (dateKey date) |> Maybe.withDefault []

        ys =
            dict |> Dict.get (weekdayKey date) |> Maybe.withDefault []

        f meta acc =
            if meta.sinceYear |> Maybe.map (\y -> year >= y) |> Maybe.withDefault True then
                let
                    key =
                        "observance." ++ meta.key

                    name =
                        if I18Next.hasKey translations key then
                            I18Next.t translations key

                        else
                            meta.key
                in
                name :: acc

            else
                acc
    in
    ys |> List.foldl f (xs |> List.foldl f [])


dateKey : Date -> Int
dateKey date =
    dateToInt <| DateOfYear (Date.monthNumber date - 1) (Date.day date)


weekdayKey : Date -> Int
weekdayKey date =
    weekdayToInt <|
        WeekdayOfMonth
            (Date.monthNumber date - 1)
            ((Date.day date - 1) // 7)
            (Date.weekdayNumber date |> remainderBy 7)


keyToInt : Key -> Int
keyToInt key =
    case key of
        Date date ->
            dateToInt date

        Weekday weekday ->
            weekdayToInt weekday


dateToInt : DateOfYear -> Int
dateToInt { month, date } =
    31 * month + date - 1


weekdayToInt : WeekdayOfMonth -> Int
weekdayToInt { month, week, day } =
    31 * 12 + 7 * 5 * month + 7 * week + day - 1
