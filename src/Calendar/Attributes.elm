module Calendar.Attributes exposing
    ( ariaBusy
    , ariaChecked
    , ariaControls
    , ariaDescribedby
    , ariaExpanded
    , ariaHaspopup
    , ariaHidden
    , ariaKeyshortcuts
    , ariaLabel
    , ariaLabelledby
    , ariaLive
    , ariaModal
    , ariaOrientation
    , loading
    , role
    , xmlns
    )

import Html exposing (Attribute)
import Html.Attributes exposing (attribute, property)
import Json.Encode as Json


boolStringAttribute : String -> Bool -> Attribute msg
boolStringAttribute name value =
    attribute name <|
        if value then
            "true"

        else
            "false"


ariaBusy : Bool -> Attribute msg
ariaBusy =
    boolStringAttribute "aria-busy"


ariaChecked : Bool -> Attribute msg
ariaChecked =
    -- We are not using the `mixed` value now so let's ignore it.
    boolStringAttribute "aria-checked"


ariaControls : String -> Attribute msg
ariaControls =
    attribute "aria-controls"


ariaDescribedby : String -> Attribute msg
ariaDescribedby =
    attribute "aria-describedby"


ariaExpanded : Bool -> Attribute msg
ariaExpanded =
    boolStringAttribute "aria-expanded"


ariaHaspopup : String -> Attribute msg
ariaHaspopup =
    attribute "aria-haspopup"


ariaHidden : Bool -> Attribute msg
ariaHidden =
    boolStringAttribute "aria-hidden"


ariaKeyshortcuts : String -> Attribute msg
ariaKeyshortcuts =
    attribute "aria-keyshortcuts"


ariaLabel : String -> Attribute msg
ariaLabel =
    attribute "aria-label"


ariaLabelledby : String -> Attribute msg
ariaLabelledby =
    attribute "aria-labelledby"


ariaLive : String -> Attribute msg
ariaLive =
    attribute "aria-live"


ariaModal : Bool -> Attribute msg
ariaModal =
    boolStringAttribute "aria-modal"


ariaOrientation : String -> Attribute msg
ariaOrientation =
    attribute "aria-orientation"


loading : String -> Attribute msg
loading value =
    property "loading" (Json.string value)


role : String -> Attribute msg
role =
    attribute "role"


xmlns : String -> Attribute msg
xmlns =
    attribute "xmlns"
