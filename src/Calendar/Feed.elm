module Calendar.Feed exposing (Feed, Preset, decoder, presets)

import Calendar.Event exposing (Event)
import Calendar.Util.Duration as Duration
import Json.Decode as D
import List.Extra
import Time


type alias Feed =
    { title : String
    , alternate : String
    , events : List Event
    }


type alias Preset =
    { url : String
    , title : String
    , icon : String
    }


type alias Entry =
    { name : String
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
    [ Preset "UCEOugXOAfa-HRmRjKbH8z3Q.json"
        "けものフレンズプロジェクト公式"
        "https://yt3.ggpht.com/ryEtopDlUPjueM1j3UufZ3UrGCpuYxc5tdeX5-pTlkjygXqbw7j29bFIPu8uCy4NzHAM1EetmLM=s60"
    , Preset "UCmYO-WfY7Tasry4D1YB4LJw.json"
        "フンボルトペンギン / Humboldt Penguin"
        "https://yt3.ggpht.com/ytc/AKedOLSr75ivVQI4bHcaoMOaYxPjbRnL3-2VCSNuHbJ-=s60"
    , Preset "UCMpw36mXEu3SLsqdrJxUKNA.json"
        "シマハイイロギツネ / Island Fox"
        "https://yt3.ggpht.com/2ohbFqFqLbEw66rWMhTjb-wpa5X9APonb1KZiiBJbmGcS69yKUwtmLSHfhPUSF4snFp1O9r_=s60"
    , Preset "UCabMjG8p6G5xLkPJgEoTnDg.json"
        "コヨーテ / Coyote"
        "https://yt3.ggpht.com/VwLc2w11lh_JlrsDB9P4OvBJpqaoAZdS08gqQx7vtJ5-4DjsEiP5Un6xmT0q8VE6zr8uXYEnTqg=s60"
    , Preset "UCdNBhcAohYjXlUVYsz8X2KQ.json"
        "ダイアウルフ / Dire Wolf"
        "https://yt3.ggpht.com/yJUytRAl-MwBrGmIXMdctRgcNuAborghz1SGt2o6KDewrB1aP6saZLdX9HWBPdF5JEutZ6aBKNY=s60"
    , Preset "UCxm7yNjJsSvyvcG96-Cvmpw.json"
        "カラカル / Caracal"
        "https://yt3.ggpht.com/PfYRtTGfYRkpluFsqAjYgSERbvu7wQ-pNpRARUeSTvka1pa-t3rFgb4DhxNuhDikmz-BP8WLgw=s60"
    , Preset "UCEcMIuGR8WO2TwL9XIpjKtw.json"
        "ケープペンギン / African Penguin"
        "https://yt3.ggpht.com/ytc/AKedOLSiSzCCyj5TBipvNgNcz0NrPZbvJZmZQU9JUFE-=s60"
    ]


decoder : List Preset -> D.Decoder Feed
decoder meta =
    D.map3 Feed
        (D.field "title" D.string)
        (D.field "alternate" D.string)
        (D.field "entries" (D.list entryDecoder)
            |> D.map (\entries -> entries |> List.map (eventFromEntry meta))
        )


entryDecoder : D.Decoder Entry
entryDecoder =
    D.map8 Entry
        (D.field "name" D.string)
        (D.oneOf [ D.field "live" D.bool, D.succeed False ])
        (D.oneOf [ D.field "upcoming" D.bool, D.succeed False ])
        (D.field "time" D.int)
        (D.maybe (D.field "duration" D.int))
        (D.maybe (D.field "link" D.string))
        (D.maybe (D.field "thumbnail" D.string))
        (D.maybe (D.field "description" D.string))


eventFromEntry : List Preset -> Entry -> Event
eventFromEntry meta entry =
    Event entry.name
        entry.live
        entry.upcoming
        (Time.millisToPosix (entry.time * 1000))
        (entry.duration |> Maybe.map Duration.fromMillis)
        entry.link
        entry.thumbnail
        (entry.description
            |> Maybe.map (extractMembers meta)
            |> Maybe.withDefault []
        )


extractMembers : List Preset -> String -> List Int
extractMembers meta description =
    meta
        |> List.Extra.indexedFoldr
            (\i { title } members ->
                if description |> String.contains ("@" ++ title) then
                    i :: members

                else
                    members
            )
            []
