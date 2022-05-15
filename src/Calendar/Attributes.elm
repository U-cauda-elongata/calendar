module Calendar.Attributes exposing
    ( ariaBusy
    , ariaControls
    , ariaExpanded
    , ariaHaspopup
    , ariaHidden
    , ariaLabel
    , ariaLabelledby
    , ariaLive
    , loading
    , role
    , xmlns
    )

import Html exposing (Attribute)
import Html.Attributes exposing (attribute, property)
import Json.Encode as Json


boolStringAttribute : String -> Bool -> Attribute msg
boolStringAttribute name value =
    attribute name
        (if value then
            "true"

         else
            "false"
        )


ariaBusy : Bool -> Attribute msg
ariaBusy =
    boolStringAttribute "aria-busy"


ariaControls : String -> Attribute msg
ariaControls =
    attribute "aria-controls"


ariaExpanded : Bool -> Attribute msg
ariaExpanded =
    boolStringAttribute "aria-expanded"


ariaHaspopup : String -> Attribute msg
ariaHaspopup =
    attribute "aria-haspopup"


ariaHidden : Bool -> Attribute msg
ariaHidden =
    boolStringAttribute "aria-hidden"


ariaLabel : String -> Attribute msg
ariaLabel =
    attribute "aria-label"


ariaLabelledby : String -> Attribute msg
ariaLabelledby =
    attribute "aria-labelledby"


ariaLive : String -> Attribute msg
ariaLive =
    attribute "aria-live"


loading : String -> Attribute msg
loading value =
    property "loading" (Json.string value)


role : String -> Attribute msg
role =
    attribute "role"


xmlns : String -> Attribute msg
xmlns =
    attribute "xmlns"
