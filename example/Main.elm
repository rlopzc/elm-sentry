module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Http
import Json.Encode as Encode
import Sentry exposing (Sentry)
import Task
import UUID exposing (UUID)



-- MODEL


type alias Model =
    { debugStr : String
    , fatalStr : String
    }


init : ( Model, Cmd Msg )
init =
    ( { debugStr = ""
      , fatalStr = ""
      }
    , Cmd.none
    )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Sentry Example"
    , body =
        [ Html.form [ Events.onSubmit UserSubmittedFatal ]
            [ Html.fieldset []
                [ Html.legend [] [ Html.text "Fatal" ]
                , Html.input
                    [ Events.onInput UserEnteredFatalStr
                    , Attr.placeholder "Fatal"
                    , Attr.value model.fatalStr
                    ]
                    []
                , Html.input
                    [ Attr.type_ "submit"
                    , Attr.value "Submit!"
                    ]
                    []
                ]
            ]
        , Html.form [ Events.onSubmit UserSubmittedDebug ]
            [ Html.fieldset []
                [ Html.legend [] [ Html.text "Debug" ]
                , Html.input
                    [ Events.onInput UserEnteredDebugStr
                    , Attr.placeholder "Debug"
                    , Attr.value model.debugStr
                    ]
                    []
                , Html.input
                    [ Attr.type_ "submit"
                    , Attr.value "Submit!"
                    ]
                    []
                ]
            ]
        ]
    }



-- UPDATE


type Msg
    = Response (Result Http.Error UUID)
    | UserEnteredDebugStr String
    | UserSubmittedDebug
    | UserEnteredFatalStr String
    | UserSubmittedFatal


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Response _ ->
            ( model, Cmd.none )

        UserEnteredDebugStr debugStr ->
            ( { model | debugStr = debugStr }, Cmd.none )

        UserSubmittedDebug ->
            ( model
            , Task.attempt Response (sentry.debug model.debugStr Dict.empty)
            )

        UserEnteredFatalStr fatalStr ->
            ( { model | fatalStr = fatalStr }, Cmd.none )

        UserSubmittedFatal ->
            ( model
            , Task.attempt Response (sentry.fatal model.fatalStr fatalInfo)
            )


fatalInfo : Dict String Encode.Value
fatalInfo =
    Dict.fromList
        [ ( "sidebar-expanded", Encode.bool True )
        , ( "form-status", Encode.string "EditingNew" )
        , ( "errors", Encode.null )
        ]



-- SENTRY


{-| DSN: <https://1590942c446340fdacb4c9369cac2cbd@o353541.ingest.sentry.io/8242816>
-}
config : Sentry.Config
config =
    Sentry.config
        { publicKey = "1590942c446340fdacb4c9369cac2cbd"
        , host = "o353541.ingest.sentry.io"
        , projectId = "8242816"
        }


releaseVersion : Sentry.ReleaseVersion
releaseVersion =
    Sentry.releaseVersion "1.0.0"


environment : Sentry.Environment
environment =
    Sentry.environment "test"


sentry : Sentry
sentry =
    Sentry.withContext config releaseVersion environment "example"



-- MAIN


main : Program () Model Msg
main =
    Browser.document
        { init = \() -> init
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }
