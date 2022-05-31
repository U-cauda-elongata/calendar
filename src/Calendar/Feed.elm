module Calendar.Feed exposing (Feed, Preset, decoder, presets)

import Calendar.Event exposing (Event)
import Calendar.Util.Duration as Duration
import Json.Decode as D
import Time


type alias Feed =
    { meta : List Meta
    , entries : List Event
    , next : Maybe String
    }


type alias Meta =
    { id : String
    , title : String
    , alternate : String
    }


type alias Preset =
    { id : String
    , title : String
    , icon : String
    }


type alias Entry =
    { id : String
    , feed : String
    , name : String
    , live : Bool
    , upcoming : Bool
    , time : Int
    , duration : Maybe Int
    , link : Maybe String
    , thumbnail : Maybe String
    , description : Maybe String
    }


presets : List Preset
presets =
    [ Preset "yt:channel:UCEOugXOAfa-HRmRjKbH8z3Q"
        "けものフレンズプロジェクト公式"
        "https://yt3.ggpht.com/ryEtopDlUPjueM1j3UufZ3UrGCpuYxc5tdeX5-pTlkjygXqbw7j29bFIPu8uCy4NzHAM1EetmLM=s60"
    , Preset "yt:channel:UCmYO-WfY7Tasry4D1YB4LJw"
        "フンボルトペンギン / Humboldt Penguin"
        "https://yt3.ggpht.com/ytc/AKedOLSr75ivVQI4bHcaoMOaYxPjbRnL3-2VCSNuHbJ-=s60"
    , Preset "yt:channel:UCMpw36mXEu3SLsqdrJxUKNA"
        "シマハイイロギツネ / Island Fox"
        "https://yt3.ggpht.com/2ohbFqFqLbEw66rWMhTjb-wpa5X9APonb1KZiiBJbmGcS69yKUwtmLSHfhPUSF4snFp1O9r_=s60"
    , Preset "yt:channel:UCabMjG8p6G5xLkPJgEoTnDg"
        "コヨーテ / Coyote"
        "https://yt3.ggpht.com/VwLc2w11lh_JlrsDB9P4OvBJpqaoAZdS08gqQx7vtJ5-4DjsEiP5Un6xmT0q8VE6zr8uXYEnTqg=s60"
    , Preset "yt:channel:UCdNBhcAohYjXlUVYsz8X2KQ"
        "ダイアウルフ / Dire Wolf"
        "https://yt3.ggpht.com/yJUytRAl-MwBrGmIXMdctRgcNuAborghz1SGt2o6KDewrB1aP6saZLdX9HWBPdF5JEutZ6aBKNY=s60"
    , Preset "yt:channel:UCxm7yNjJsSvyvcG96-Cvmpw"
        "カラカル / Caracal"
        "https://yt3.ggpht.com/PfYRtTGfYRkpluFsqAjYgSERbvu7wQ-pNpRARUeSTvka1pa-t3rFgb4DhxNuhDikmz-BP8WLgw=s60"
    , Preset "yt:channel:UCEcMIuGR8WO2TwL9XIpjKtw"
        "ケープペンギン / African Penguin"
        "https://yt3.ggpht.com/ytc/AKedOLSiSzCCyj5TBipvNgNcz0NrPZbvJZmZQU9JUFE-=s60"
    ]


decoder : List Preset -> D.Decoder Feed
decoder presetlist =
    D.map3 Feed
        (D.oneOf [ D.field "meta" (D.list metaDecoder), D.succeed [] ])
        (D.field "entries" (D.list entryDecoder)
            |> D.map (\entries -> entries |> List.map (eventFromEntry presetlist))
        )
        (D.maybe <| D.field "next" D.string)


metaDecoder : D.Decoder Meta
metaDecoder =
    D.map3 Meta
        (D.field "id" D.string)
        (D.field "title" D.string)
        (D.field "alternate" D.string)


entryDecoder : D.Decoder Entry
entryDecoder =
    D.map Entry (D.field "id" D.string)
        |> D.andThen (\f -> D.map f <| D.field "feed" D.string)
        |> D.andThen (\f -> D.map f <| D.field "name" D.string)
        |> D.andThen (\f -> D.map f <| D.oneOf [ D.field "live" D.bool, D.succeed False ])
        |> D.andThen (\f -> D.map f <| D.oneOf [ D.field "upcoming" D.bool, D.succeed False ])
        |> D.andThen (\f -> D.map f <| D.field "time" D.int)
        |> D.andThen (\f -> D.map f <| D.maybe <| D.field "duration" D.int)
        |> D.andThen (\f -> D.map f <| D.maybe <| D.field "link" D.string)
        |> D.andThen (\f -> D.map f <| D.maybe <| D.field "thumbnail" D.string)
        |> D.andThen (\f -> D.map f <| D.maybe <| D.field "description" D.string)


eventFromEntry : List Preset -> Entry -> Event
eventFromEntry presetlist entry =
    Event entry.id
        entry.feed
        entry.name
        entry.live
        entry.upcoming
        (Time.millisToPosix (entry.time * 1000))
        (entry.duration |> Maybe.map Duration.fromMillis)
        entry.link
        entry.thumbnail
        (entry.description
            |> Maybe.map (extractMembers presetlist entry.feed)
            |> Maybe.withDefault []
        )


extractMembers : List Preset -> String -> String -> List String
extractMembers presetlist myFeed description =
    presetlist
        |> List.filterMap
            (\{ id, title } ->
                if id /= myFeed && (description |> String.contains ("@" ++ title)) then
                    Just id

                else
                    Nothing
            )
