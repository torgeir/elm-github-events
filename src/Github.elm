module Github
    exposing
        ( fetchAllEvents
        , ApiError
        , Username
        , Events
        , Event
        , EventActor
        , EventRepo
        , EventAction(..)
        , PullRequest
        )

{-| This library fetches public github events for users using the github api.

# Functions
@docs fetchAllEvents

# Errors
@docs ApiError

# Query types
@docs Username

# Response types
@docs Events, Event, EventActor, EventRepo, EventAction, PullRequest

-}

import Http exposing (..)
import Json.Decode as Json exposing ((:=))
import Task exposing (..)
import Regex


{-| Fetch all github events for a list of string usernames
-}
fetchAllEvents : (Error -> a) -> (Events -> a) -> List String -> Cmd a
fetchAllEvents failedMsg successMsg usernames =
    let
        cmds =
            List.map (fetchUserEvents failedMsg successMsg) usernames
    in
        Cmd.batch cmds


{-| Fetch github events a single string username
-}
fetchUserEvents : (Error -> a) -> (Events -> a) -> String -> Cmd a
fetchUserEvents failedMsg successMsg user =
    let
        url =
            --"http://localhost:3001/" ++ user
            "https://api.github.com/users/" ++ user ++ "/events/public"

        request =
            (Http.get decoderEvents url)
    in
        (Task.perform failedMsg successMsg request)


{-| Error response returned when api calls fail
-}
type alias ApiError =
    Http.Error


{-| Username query type
-}
type alias Username =
    String


{-| Events returned from the api
-}
type alias Events =
    List Event


{-| Event type returned from the api
-}
type alias Event =
    { id : String
    , action : EventAction
    , created_at : String
    , actor : EventActor
    , repo : EventRepo
    }


{-| EventActor type that states who triggered the event
-}
type alias EventActor =
    { display_login : String
    , url : String
    , avatar_url : String
    }


{-| EventRepo type for the repo that has been acted on and its url'en
-}
type alias EventRepo =
    { name : String
    , url : String
    }


type alias Commit =
    { sha : String
    }


type alias Commits =
    List Commit


{-| PullRequest type with extra info about pull requests
-}
type alias PullRequest =
    { action : String
    , number : Int
    , url : String
    , title : String
    }


type alias Issue =
    { number : Int
    , url : String
    , title : String
    }


type alias IssueAction =
    { action : String
    , issue : Issue
    }


type alias Comment =
    { url : String
    }


type alias IssueComment =
    { issue : Issue
    , comment : Comment
    }


{-| EventAction types that represent the action that happened in the event api
-}
type EventAction
    = EventActionPush EventActionPushPayload
    | EventActionFork
    | EventActionPullRequestReviewComment Comment PullRequest
    | EventActionPullRequest PullRequest
    | EventActionIssueComment IssueComment
    | EventActionIssue IssueAction
    | EventActionCommitComment Comment
    | EventActionWatch
    | EventActionDelete EventActionDeletePayload
    | EventActionMember String String
    | EventActionCreate (Maybe String) String (Maybe String)
    | EventActionUnknown String


type alias EventActionDeletePayload =
    { ref : String
    , ref_type : String
    }


type alias EventActionPushPayload =
    { head : String
    , commits : Commits
    }


decoderEvents : Json.Decoder (List Event)
decoderEvents =
    Json.list decoderEvent


decoderEvent : Json.Decoder Event
decoderEvent =
    Json.object5 Event
        ("id" := Json.string)
        (("type" := Json.string) `Json.andThen` decoderEventAction)
        ("created_at" := Json.string)
        ("actor" := decoderEventActor)
        ("repo" := decoderEventRepo)


decoderPullRequest : Json.Decoder PullRequest
decoderPullRequest =
    (Json.object4 PullRequest
        (Json.at [ "payload", "action" ] Json.string)
        (Json.at [ "payload", "pull_request", "number" ] Json.int)
        (Json.at [ "payload", "pull_request", "html_url" ] Json.string)
        (Json.at [ "payload", "pull_request", "title" ] Json.string)
    )


decoderIssue : Json.Decoder Issue
decoderIssue =
    (Json.object3 Issue
        (Json.at [ "payload", "issue", "number" ] Json.int)
        (Json.at [ "payload", "issue", "html_url" ] Json.string)
        (Json.at [ "payload", "issue", "title" ] Json.string)
    )


decoderComment : Json.Decoder Comment
decoderComment =
    Json.object1 Comment
        (Json.at [ "payload", "comment", "html_url" ] Json.string)


decoderEventAction : String -> Json.Decoder EventAction
decoderEventAction type' =
    case type' of
        "PullRequestEvent" ->
            Json.object1 EventActionPullRequest
                decoderPullRequest

        "PushEvent" ->
            Json.object1 EventActionPush
                ("payload" := decoderPayloadPush)

        "CreateEvent" ->
            Json.object3 EventActionCreate
                (Json.maybe (Json.at [ "payload", "ref" ] Json.string))
                (Json.at [ "payload", "ref_type" ] Json.string)
                (Json.maybe (Json.at [ "payload", "description" ] Json.string))

        "ForkEvent" ->
            Json.succeed EventActionFork

        "PullRequestReviewCommentEvent" ->
            Json.object2 EventActionPullRequestReviewComment
                decoderComment
                decoderPullRequest

        "CommitCommentEvent" ->
            Json.object1 EventActionCommitComment
                decoderComment

        "IssueCommentEvent" ->
            Json.object1 EventActionIssueComment
                (Json.object2 IssueComment
                    decoderIssue
                    decoderComment
                )

        "IssuesEvent" ->
            Json.object1 EventActionIssue
                (Json.object2 IssueAction
                    (Json.at [ "payload", "action" ] Json.string)
                    decoderIssue
                )

        "MemberEvent" ->
            Json.object2 EventActionMember
                (Json.at [ "payload", "action" ] Json.string)
                (Json.at [ "payload", "member", "login" ] Json.string)

        "WatchEvent" ->
            Json.succeed EventActionWatch

        "DeleteEvent" ->
            Json.object1 EventActionDelete
                ("payload" := decoderPayloadDelete)

        _ ->
            Json.succeed (Debug.log ("Unhandled github event type " ++ type' ++ ", don't have a text to show for it") EventActionUnknown type')


decoderEventActor : Json.Decoder EventActor
decoderEventActor =
    Json.object3 EventActor
        ("display_login" := Json.string)
        ("url" := Json.string `Json.andThen` makeUrlBrowserNavigable)
        ("avatar_url" := Json.string `Json.andThen` makeUrlBrowserNavigable)


decoderEventRepo : Json.Decoder EventRepo
decoderEventRepo =
    Json.object2 EventRepo
        ("name" := Json.string)
        ("url" := Json.string `Json.andThen` makeUrlBrowserNavigable)


makeUrlBrowserNavigable : String -> Json.Decoder String
makeUrlBrowserNavigable url =
    Json.succeed
        (Regex.replace
            (Regex.AtMost 1)
            (Regex.regex "api.github.com/(.*?)/")
            (\_ -> "github.com/")
            url
        )


decoderPayloadPush : Json.Decoder EventActionPushPayload
decoderPayloadPush =
    Json.object2 EventActionPushPayload
        ("head" := Json.string)
        ("commits" := decoderCommits)


decoderPayloadDelete : Json.Decoder EventActionDeletePayload
decoderPayloadDelete =
    Json.object2 EventActionDeletePayload
        ("ref" := Json.string)
        ("ref_type" := Json.string)


decoderCommits : Json.Decoder Commits
decoderCommits =
    Json.list decoderCommit


decoderCommit : Json.Decoder Commit
decoderCommit =
    Json.object1 Commit
        ("sha" := Json.string)
