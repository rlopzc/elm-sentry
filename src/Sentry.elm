module Sentry exposing
    ( Sentry, Level(..), Config, config, Environment, environment, Context, context, ReleaseVersion, releaseVersion
    , send, withContext
    )

{-| Sentry SDK


## Types

@docs Sentry, Level, Config, config, Environment, environment, Context, context, ReleaseVersion, releaseVersion


## Reporting errors

@docs send, withContext

-}

import Dict exposing (Dict)
import Http
import Json.Encode as Encode
import Process
import Random
import Sentry.Internal as Internal
import Task exposing (Task)
import Time
import UUID exposing (UUID)


{-| Record that contains functions to report errors by Level.

Create one using [`withContext`](#withContext).

-}
type alias Sentry =
    { fatal : String -> Dict String Encode.Value -> Task Http.Error UUID
    , error : String -> Dict String Encode.Value -> Task Http.Error UUID
    , warning : String -> Dict String Encode.Value -> Task Http.Error UUID
    , info : String -> Dict String Encode.Value -> Task Http.Error UUID
    , debug : String -> Dict String Encode.Value -> Task Http.Error UUID
    }


{-| The event severity.
-}
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


{-| The environment name, such as "production" or "staging".

Create one using [`environment`](#environment).

-}
type Environment
    = Environment String


{-| Create an [`Environment`](#Environment).

    Sentry.environment "production"

-}
environment : String -> Environment
environment =
    Environment


{-| The release version of the application.

This value can be a git SHA, or a product identifier with a semantic version.

Create one using [`releaseVersion`](#releaseVersion).

-}
type ReleaseVersion
    = ReleaseVersion String


{-| Create a [`ReleaseVersion`](#ReleaseVersion).

    Sentry.releaseVersion "721e41770371db95eee98ca2707686226b993eda"

-}
releaseVersion : String -> ReleaseVersion
releaseVersion =
    ReleaseVersion


{-| The Context, for example, the Page name.

Crate one using [`context`](#context).

-}
type Context
    = Context String


{-| Create a [`Context`](#Context).

    Sentry.context "profile/settings"

-}
context : String -> Context
context =
    Context



-- Sentry Config


type PublicKey
    = PublicKey String


type Host
    = Host String


type ProjectId
    = ProjectId String


{-| The configuration of the SDK.

Specifies the Public Key, the Host, and the Project ID.

Create one using [`config`](#config).

-}
type Config
    = Config PublicKey Host ProjectId


{-| Creates a [`Config`](#Config).

Use the dsn to provide the publicKey, host and projectId.

    -- Example: https://1900942c246350fdacb4c9369cac2ets@o298593.ingest.sentry.io/2312456
    Sentry.config
        { publicKey = "1900942c246350fdacb4c9369cac2ets"
        , host = "o298593.ingest.sentry.io"
        , projectId = "2312456"
        }

-}
config :
    { publicKey : String
    , host : String
    , projectId : String
    }
    -> Config
config conf =
    Config (PublicKey conf.publicKey) (Host conf.host) (ProjectId conf.projectId)


{-| Build a [`Sentry`](#Sentry) record configured with the given
[`Config`](#Config), [`ReleaseVersion`](#ReleaseVersion), an
[`Environment`](#Environment) and a [`Context`](#Context) string.

        import Sentry exposing (Sentry)

        config : Sentry.Config
        config =
            Sentry.config
                { publicKey = "1900942c246350fdacb4c9369cac2ets"
                , host = "o298593.ingest.sentry.io"
                , projectId = "2312456"
                }

        releaseVersion : Sentry.ReleaseVersion
        releaseVersion =
            Sentry.releaseVersion "721e41770371db95eee98ca2707686226b993eda"

        environment : Sentry.Environment
        environment =
            Sentry.environment "production"

        sentry : Sentry
        sentry =
            Sentry.withContext config releaseVersion environment "profile/settings"

        sentry.fatal "Backend on fire!" (Dict.fromList [("response", toString response)])
        sentry.debug "Testing sentry integration" Dict.empty
        sentry.info "Backend timeout when saving the profile settings" Dict.empty

-}
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


{-| Send an error to Sentry.

Normally, it's preferable to use [`withContext`](#withContext)
as it provides a reusable configured [`Sentry`](#Sentry).

Arguments:

  - [`Config`](#Config): The configuration about your Sentry team and project. Create one using [`config`](#config)
  - [`Level`](#Level): The severity of the error, e.g. `Fatal`, `Error`, `Info`
  - [`ReleaseVersion`](#ReleaseVersion): Your app version. Can be a git commit SHA.
  - [`Environment`](#Environment): e.g. `"production"`, `"staging"`
  - [`Context`](#Context): Scopes the message. It can be the name of the Page that produced this error, e.g. `"profile/settings"`
  - `String`: The error message, e.g. `"Backend on fire!"`
  - `Dict String Value`: Extra information, e.g. `{"response": "503 Service unavailable"}`

On success, the [`Task`](http://package.elm-lang.org/packages/elm-lang/core/latest/Task#Task)
will resolve to a [`UUID`](https://package.elm-lang.org/packages/TSFoster/elm-uuid/latest/UUID#UUID)
that was sent to Sentry as the Event ID.

On failure, the [`Http.Error`](http://package.elm-lang.org/packages/elm-lang/http/latest/Http#Error) will
hold the information about the problem.

-}
send : Config -> Level -> ReleaseVersion -> Environment -> Context -> String -> Dict String Encode.Value -> Task Http.Error UUID
send vconfig level vreleaseVersion venvironment vcontext message metadata =
    Time.now
        |> Task.andThen (sendWithTime vconfig level vreleaseVersion venvironment vcontext message metadata)


sendWithTime : Config -> Level -> ReleaseVersion -> Environment -> Context -> String -> Dict String Encode.Value -> Time.Posix -> Task Http.Error UUID
sendWithTime (Config vpublicKey vhost vprojectId) level vreleaseVersion venvironment vcontext message metadata posix =
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
    , url = endpointUrl vhost vprojectId
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


{-| <https://develop.sentry.dev/sdk/event-payloads/>
-}
toJsonBody : UUID -> Time.Posix -> Level -> ReleaseVersion -> Environment -> Context -> String -> Dict String Encode.Value -> Http.Body
toJsonBody uuid posix level (ReleaseVersion vreleaseVersion) (Environment venvironment) (Context vcontext) message metadata =
    [ ( "event_id", Encode.string (UUID.toRepresentation UUID.Compact uuid) )
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



-- Internal


{-| The message interface encoder

<https://develop.sentry.dev/sdk/event-payloads/message>

-}
messageEncoder : String -> Encode.Value
messageEncoder vmessage =
    Encode.object
        [ ( "formatted", Encode.string vmessage )
        ]


{-| <https://develop.sentry.dev/sdk/overview/#authentication>

    X-Sentry-Auth: Sentry sentry_version=7,
    sentry_client=<client version, arbitrary>,
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


{-| <https://develop.sentry.dev/sdk/overview/#parsing-the-dsn>
-}
endpointUrl : Host -> ProjectId -> String
endpointUrl (Host vhost) (ProjectId vprojectId) =
    "https://" ++ vhost ++ "/api/" ++ vprojectId ++ "/store/"


retries : { maxAttempts : Int, msDelayBetweenRetries : Float }
retries =
    { maxAttempts = 60
    , msDelayBetweenRetries = 1000
    }


{-| Turn a Posix.Time into the number of seconds since Epoch.
-}
posixToSeconds : Time.Posix -> Int
posixToSeconds posix =
    Time.posixToMillis posix // 1000
