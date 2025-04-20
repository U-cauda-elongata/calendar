module InteropDefinitions exposing (Features, Flags, FromElm(..), PresetFeedMeta, ShareData, ToElm(..), interop)

import Json.Decode
import TsJson.Decode as D exposing (Decoder)
import TsJson.Encode as E exposing (Encoder)


interop :
    { toElm : Decoder ToElm
    , fromElm : Encoder FromElm
    , flags : Decoder Flags
    }
interop =
    { toElm = toElm
    , fromElm = fromElm
    , flags = flagsDecoder
    }


type FromElm
    = Close String
    | Copy String
    | PreventScrollFocus String
    | RemoveScrollEventListener
    | SetLang String
    | Share ShareData
    | ShowModal String
    | SlideViewportInto String


type alias ShareData =
    { title : String
    , url : Maybe String
    }


type ToElm
    = OnScrollToBottom


type alias Flags =
    { features : Features
    , languages : List String
    , feeds : List PresetFeedMeta
    , observances : Json.Decode.Value
    }


type alias Features =
    { copy : Bool
    , share : Bool
    }


type alias PresetFeedMeta =
    { id : String
    , title : String
    , lang : String
    , icon : Icon
    }


type alias Icon =
    { url : String
    , width : Int
    , height : Int
    }


fromElm : Encoder FromElm
fromElm =
    E.union
        (\vClose vCopy vPreventScrollFocus vRemoveScrollEventListener vSetLang vShare vShowModal vSlideViewportInto value ->
            case value of
                Close id ->
                    vClose id

                Copy text ->
                    vCopy text

                PreventScrollFocus id ->
                    vPreventScrollFocus id

                RemoveScrollEventListener ->
                    vRemoveScrollEventListener

                SetLang lang ->
                    vSetLang lang

                Share data ->
                    vShare data

                ShowModal id ->
                    vShowModal id

                SlideViewportInto id ->
                    vSlideViewportInto id
        )
        |> E.variantTagged "close" E.string
        |> E.variantTagged "copy" E.string
        |> E.variantTagged "preventScrollFocus" E.string
        |> E.variant0 "removeScrollEventListener"
        |> E.variantTagged "setLang" E.string
        |> E.variantTagged "share"
            (E.object
                [ E.required "title" .title E.string
                , E.optional "url" .url E.string
                ]
            )
        |> E.variantTagged "showModal" E.string
        |> E.variantTagged "slideViewportInto" E.string
        |> E.buildUnion


toElm : Decoder ToElm
toElm =
    D.discriminatedUnion "tag"
        [ ( "OnScrollToBottom"
          , D.succeed OnScrollToBottom
          )
        ]


flagsDecoder : Decoder Flags
flagsDecoder =
    D.map4 Flags
        (D.field "features" <| featuresDecoder)
        (D.field "languages" <| D.list <| D.string)
        (D.field "feeds" <| D.list <| presetFeedMetaDecoder)
        (D.field "observances" D.value)


featuresDecoder : Decoder Features
featuresDecoder =
    D.map2 Features
        (D.field "copy" D.bool)
        (D.field "share" D.bool)


presetFeedMetaDecoder : Decoder PresetFeedMeta
presetFeedMetaDecoder =
    D.map4 PresetFeedMeta
        (D.field "id" D.string)
        (D.field "title" D.string)
        (D.field "lang" D.string)
        (D.field "icon" iconDecoder)


iconDecoder : Decoder Icon
iconDecoder =
    D.map3 Icon
        (D.field "url" D.string)
        (D.field "width" D.int)
        (D.field "height" D.int)
