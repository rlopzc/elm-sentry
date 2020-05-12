module Sentry exposing
    ( Config
    , Context
    , Environment
    , ProjectId
    , PublicKey
    , ReleaseVersion
    , Sentry
    , config
    , environment
    , projectId
    , publicKey
    , releaseVersion
    , scope
    )

import Dict exposing (Dict)
import Http
import Json.Encode as Encode
import Sentry.Internal as Internal
import Task exposing (Task)
import Time



{-
   https://develop.sentry.dev/sdk/event-payloads/
      Required attributes for the Events
       - event_id: Hexadecimal string representing a uuid4 value. The length is exactly 32 characters. Dashes are not allowed.
       - timestamp: Unix timestamp. Time.posixToMillis // 1000
       - logger: Name of the logger. Project name?
       - platform: "elm"

       -- Optionals
       - level: fatal, error, warning, info, debug
       - release: Version of the application. Commit SHA
       - environment: production, staging
       - extra: An arbitrary mapping of additional metadata to store with the event.
       - tags:
          - context: Can be the page name
-}
-- TODO: Use UUID instead of String?


type alias Sentry =
    { fatal : String -> Dict String Encode.Value -> Task Http.Error String
    , error : String -> Dict String Encode.Value -> Task Http.Error String
    , warning : String -> Dict String Encode.Value -> Task Http.Error String
    , info : String -> Dict String Encode.Value -> Task Http.Error String
    , debug : String -> Dict String Encode.Value -> Task Http.Error String
    }


type Level
    = Fatal
    | Error
    | Warning
    | Info
    | Debug


levelToString : Level -> String
levelToString level =
    case level of
        Fatal ->
            "fatal"

        Error ->
            "error"

        Warning ->
            "warning"

        Info ->
            "info"

        Debug ->
            "debug"


type Environment
    = Environment String


environment : String -> Environment
environment =
    Environment


type ReleaseVersion
    = ReleaseVersion String


releaseVersion : String -> ReleaseVersion
releaseVersion =
    ReleaseVersion


{-| Context is going to be sent as Tag
<https://docs.sentry.io/enriching-error-data/additional-data/>
tags: {
context: ""
}

That's why the scoped fn is useful. You define the scope in the page and use that to sent logs

-}
type Context
    = Context String


scope : String -> Context
scope =
    Context



-- Sentry Config


type PublicKey
    = PublicKey String


publicKey : String -> PublicKey
publicKey =
    PublicKey


type ProjectId
    = ProjectId String


projectId : String -> ProjectId
projectId =
    ProjectId


type Config
    = Config PublicKey ProjectId


config :
    { publicKey : PublicKey
    , projectId : ProjectId
    }
    -> Config
config conf =
    Config conf.publicKey conf.projectId


toJsonBody : String -> Time.Posix -> Level -> ReleaseVersion -> Environment -> Context -> String -> Dict String Encode.Value -> Http.Body
toJsonBody uuid posix level (ReleaseVersion vreleaseVersion) (Environment venvironment) (Context vcontext) message metadata =
    [ ( "event_id", Encode.string uuid )
    , ( "timestamp", Encode.int <| posixToSeconds posix )
    , ( "plaform", Encode.string "elm" )
    , ( "level", Encode.string (levelToString level) )
    , ( "release", Encode.string vreleaseVersion )
    , ( "environment", Encode.string venvironment )
    , ( "tags"
      , Encode.object
            [ ( "context", Encode.string vcontext ) ]
      )
    , ( "message", messageEncoder message )
    , ( "extra", Encode.object (Dict.toList metadata) )
    ]
        |> Encode.object
        |> Http.jsonBody



-- Internal


{-| The message interface encoder

<https://develop.sentry.dev/sdk/event-payloads/message>

-}
messageEncoder : String -> Encode.Value
messageEncoder vmessage =
    Encode.object
        [ ( "formatted", Encode.string vmessage )
        ]



{- https://develop.sentry.dev/sdk/overview/#authentication
   X-Sentry-Auth: Sentry sentry_version=7,
     sentry_client=<client version, arbitrary>, version of this client: 0.0.1
     sentry_timestamp=<current timestamp>,
     sentry_key=<public api key>,
     sentry_secret=<secret api key>
-}


authHeader : Time.Posix -> Config -> Http.Header
authHeader posix (Config (PublicKey vpublicKey) _) =
    Http.header "X-Sentry-Auth" <|
        ("Sentry sentry_version=" ++ sentryVersion)
            ++ (",sentry_client=elm-sentry/" ++ Internal.version)
            ++ (",sentry_timestamp=" ++ String.fromInt (posixToSeconds posix))
            ++ (",sentry_key=" ++ vpublicKey)


sentryVersion : String
sentryVersion =
    "7"


endpointUrl : Config -> String
endpointUrl (Config _ (ProjectId vprojectId)) =
    "https://sentry.com/api/" ++ vprojectId ++ "/store/"


{-| Turn a Posix.Time into the number of seconds since Epoch
-}
posixToSeconds : Time.Posix -> Int
posixToSeconds posix =
    Time.posixToMillis posix // 1000
