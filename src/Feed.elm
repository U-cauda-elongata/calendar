module Feed exposing (Feed, Meta, Preset, decoder)

import Event exposing (Event)
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
        (D.field "entries" <| D.list Event.decoder)
        (D.maybe <| D.field "next" D.string)


metaDecoder : D.Decoder Meta
metaDecoder =
    D.map3 Meta
        (D.field "id" D.string)
        (D.field "title" D.string)
        (D.field "alternate" D.string)
