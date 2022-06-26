module Feed exposing (Feed, Meta, Preset, decoder)

import EventCore as Event exposing (Event)
import Json.Decode as D


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
    , lang : String
    , icon : String
    }


decoder : D.Decoder Feed
decoder =
    D.map3 Feed
        (D.oneOf [ D.field "meta" (D.list metaDecoder), D.succeed [] ])
        -- TODO: Implement some fancy representations for collab streams.
        (D.field "entry_groups"
            (D.list (D.oneOf [ Event.decoder |> D.map List.singleton, D.list Event.decoder ])
                |> D.map List.concat
            )
        )
        (D.maybe <| D.field "next" D.string)


metaDecoder : D.Decoder Meta
metaDecoder =
    D.map3 Meta
        (D.field "id" D.string)
        (D.field "title" D.string)
        (D.field "alternate" D.string)
