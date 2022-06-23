module Event exposing (Event, decoder, isOngoing, isShown)

import EventCore as Core
import Filter exposing (Filter)
import Json.Decode as D
import Search


type alias Event =
    Core.Event


decoder : D.Decoder Event
decoder =
    Core.decoder


isOngoing : Event -> Bool
isOngoing event =
    not event.upcoming && event.duration == Nothing


isShown : Filter -> Filter.Feed -> Event -> Bool
isShown filter feed event =
    (feed.checked
        || -- Check that any of the members' feed is checked.
           (filter.feeds
                |> List.any (\f -> f.checked && (event.members |> List.member f.preset.id))
           )
    )
        && Search.matches filter.q event.name
