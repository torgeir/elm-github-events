module App exposing (..)

import Github
import Entries
import Html exposing (div, text)


type alias Model =
    Entries.Model


type Action
    = FetchEventsResultAction Github.Events
    | FetchEventsFailedAction Github.ApiError


users : List String
users =
    [ "torgeir"
    , "emilmork"
    , "mikaelbr"
    ]


initialModel : Model
initialModel =
    []


subscriptions : Model -> Sub Action
subscriptions model =
    Sub.none


init : ( Model, Cmd Action )
init =
    ( initialModel, (Github.fetchAllEvents FetchEventsFailedAction FetchEventsResultAction users) )


view : Model -> Html.Html msg
view model =
    div [] (List.map Entries.view model)


joinEntries : Model -> Model -> Model
joinEntries entries moreEntries =
    entries
        |> List.append moreEntries
        |> List.sortBy .timestamp
        |> List.reverse


eventToEntry : Github.Event -> Entries.Entry
eventToEntry event =
    { action = event.action
    , user = Entries.Profile event.actor.display_login event.actor.url
    , avatar = event.actor.avatar_url
    , timestamp = event.created_at
    , repo = Entries.Repo event.repo.name event.repo.url
    }


update : Action -> Model -> ( Model, Cmd Action )
update msg model =
    case msg of
        FetchEventsFailedAction error ->
            let
                errorText =
                    (toString error)
            in
                Debug.log
                    errorText
                    ( model, Cmd.none )

        FetchEventsResultAction eventList ->
            ( (joinEntries model (List.map eventToEntry eventList)), Cmd.none )
