module Calendar.Event exposing (Event, isOngoing)

import Calendar.Util as Util
import Calendar.Util.Duration exposing (Duration)
import Time


type alias Event =
    { id : String
    , feed : String
    , name : String
    , live : Bool
    , upcoming : Bool
    , time : Time.Posix
    , duration : Maybe Duration
    , link : Maybe String
    , thumbnail : Maybe String
    , members : List String
    }


isOngoing : Event -> Bool
isOngoing event =
    not event.upcoming && Util.isNothing event.duration
