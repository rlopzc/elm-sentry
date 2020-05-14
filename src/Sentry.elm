module Sentry exposing
    ( Config
    , Context
    , Environment
    , Level(..)
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
    , send
    , withContext
    )

import Dict exposing (Dict)
import Http
import Json.Encode as Encode
import Process
import Random
import Sentry.Internal as Internal
import Task exposing (Task)
import Time
import UUID exposing (UUID)



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
    { fatal : String -> Dict String Encode.Value -> Task Http.Error UUID
    , error : String -> Dict String Encode.Value -> Task Http.Error UUID
    , warning : String -> Dict String Encode.Value -> Task Http.Error UUID
    , info : String -> Dict String Encode.Value -> Task Http.Error UUID
    , debug : String -> Dict String Encode.Value -> Task Http.Error UUID
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


withContext : Config -> ReleaseVersion -> Environment -> String -> Sentry
withContext vconfig vreleaseVersion venvironment contextStr =
    let
        vcontext =
            Context contextStr
    in
    { fatal = send vconfig Fatal vreleaseVersion venvironment vcontext
    , error = send vconfig Error vreleaseVersion venvironment vcontext
    , warning = send vconfig Warning vreleaseVersion venvironment vcontext
    , info = send vconfig Info vreleaseVersion venvironment vcontext
    , debug = send vconfig Debug vreleaseVersion venvironment vcontext
    }


send : Config -> Level -> ReleaseVersion -> Environment -> Context -> String -> Dict String Encode.Value -> Task Http.Error UUID
send vconfig level vreleaseVersion venvironment vcontext message metadata =
    Time.now
        |> Task.andThen (sendWithTime vconfig level vreleaseVersion venvironment vcontext message metadata)


sendWithTime : Config -> Level -> ReleaseVersion -> Environment -> Context -> String -> Dict String Encode.Value -> Time.Posix -> Task Http.Error UUID
sendWithTime (Config vpublicKey vprojectId) level vreleaseVersion venvironment vcontext message metadata posix =
    let
        uuid : UUID
        uuid =
            posixToSeconds posix
                |> Random.initialSeed
                |> Random.step UUID.generator
                |> Tuple.first

        body : Http.Body
        body =
            toJsonBody uuid posix level vreleaseVersion venvironment vcontext message metadata
    in
    { method = "POST"
    , headers = [ authHeader posix vpublicKey ]
    , url = endpointUrl vprojectId
    , body = body
    , resolver = Http.stringResolver (\_ -> Ok ())
    , timeout = Nothing
    }
        |> Http.task
        |> Task.map (\() -> uuid)
        |> withRetry retries.maxAttempts


withRetry : Int -> Task Http.Error a -> Task Http.Error a
withRetry maxRetryAttempts task =
    let
        retry : Http.Error -> Task Http.Error a
        retry httpError =
            if maxRetryAttempts > 0 then
                case httpError of
                    Http.BadStatus statusCode ->
                        -- Retry in case we hit a rate-limit
                        if statusCode == 429 then
                            Process.sleep retries.msDelayBetweenRetries
                                |> Task.andThen (\() -> withRetry (maxRetryAttempts - 1) task)

                        else
                            Task.fail httpError

                    _ ->
                        Task.fail httpError

            else
                Task.fail httpError
    in
    Task.onError retry task


toJsonBody : UUID -> Time.Posix -> Level -> ReleaseVersion -> Environment -> Context -> String -> Dict String Encode.Value -> Http.Body
toJsonBody uuid posix level (ReleaseVersion vreleaseVersion) (Environment venvironment) (Context vcontext) message metadata =
    [ ( "event_id", Encode.string (uuidRemoveDashes uuid) )
    , ( "timestamp", Encode.int (posixToSeconds posix) )
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


uuidRemoveDashes : UUID -> String
uuidRemoveDashes =
    UUID.toString >> String.replace "-" ""



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


authHeader : Time.Posix -> PublicKey -> Http.Header
authHeader posix (PublicKey vpublicKey) =
    Http.header "X-Sentry-Auth" <|
        ("Sentry sentry_version=" ++ sentryVersion)
            ++ (",sentry_client=elm-sentry/" ++ Internal.version)
            ++ (",sentry_timestamp=" ++ String.fromInt (posixToSeconds posix))
            ++ (",sentry_key=" ++ vpublicKey)


sentryVersion : String
sentryVersion =
    "7"


endpointUrl : ProjectId -> String
endpointUrl (ProjectId vprojectId) =
    "https://sentry.com/api/" ++ vprojectId ++ "/store/"


retries : { maxAttempts : Int, msDelayBetweenRetries : Float }
retries =
    { maxAttempts = 60
    , msDelayBetweenRetries = 1000
    }


{-| Turn a Posix.Time into the number of seconds since Epoch
-}
posixToSeconds : Time.Posix -> Int
posixToSeconds posix =
    Time.posixToMillis posix // 1000
