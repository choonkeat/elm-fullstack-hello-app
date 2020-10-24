port module Server exposing (..)

import Fullstack.Server
import Fullstack.Server.HTTP exposing (Method(..), StatusCode(..))
import Json.Decode
import Json.Encode
import Platform exposing (Task)
import Task
import Time
import Types exposing (MsgFromServer)
import Url



-- port onWebsocketEvent : (Json.Encode.Value -> msg) -> Sub msg
--
--
-- port writeWs : Json.Encode.Value -> Cmd msg
--
--
-- writeWebsocketMessage =
--     Fullstack.Server.writeWebsocketMessage writeWs


port onHttpRequest : (Json.Encode.Value -> msg) -> Sub msg


port onHttpResponse : Json.Encode.Value -> Cmd msg


writeResponse =
    Fullstack.Server.writeResponse onHttpResponse


main =
    Fullstack.Server.worker
        { worker =
            { init = init
            , update = update
            , subscriptions = subscriptions
            }
        , ports =
            { writeResponse = writeResponse
            , onHttpRequest = onHttpRequest
            , onWebsocketEvent = \_ -> Sub.none -- onWebsocketEvent
            , writeWebsocketMessage = \_ _ _ -> Cmd.none -- writeWebsocketMessage
            }
        , protocol =
            { routeDecoder = routeDecoder
            , updateFromRoute = updateFromRoute
            , updateFromClient = updateFromClient
            , serverMsgEncoder = Types.encodeTypesMsgFromServer
            , clientMsgDecoder = Types.decodeTypesMsgFromClient
            , headerDecoder = headerDecoder
            , errorEncoder = Json.Encode.string
            }
        }


type alias Flags =
    {}


type alias ServerState =
    { greeting : String
    }


type Msg
    = Msg



-- Platform.worker


init : Flags -> ( ServerState, Cmd Msg )
init flags =
    let
        serverState =
            { greeting = "Hello world" }

        cmd =
            Cmd.none
    in
    ( serverState, cmd )


update : Msg -> ServerState -> ( ServerState, Cmd Msg )
update msg serverState =
    case msg of
        Msg ->
            ( serverState, Cmd.none )


subscriptions : ServerState -> Sub Msg
subscriptions serverState =
    Sub.none



-- Server-side route and update function


type Route
    = Homepage


routeDecoder : Url.Url -> Maybe Route
routeDecoder urlUrl =
    case urlUrl.path of
        "/" ->
            Just Homepage

        _ ->
            Nothing


updateFromRoute : ( Fullstack.Server.HTTP.Method, Types.RequestContext, Maybe Route ) -> Time.Posix -> Fullstack.Server.HTTP.Request -> ServerState -> ( ServerState, Cmd Msg )
updateFromRoute ( method, ctx, route ) now request serverState =
    case ( method, ctx, route ) of
        ( GET, _, _ ) ->
            ( serverState
            , writeResponse request
                { statusCode = StatusOK
                , body = spaHtml
                , headers =
                    [ ( "Content-Type", Json.Encode.string "text/html; charset=utf-8" )
                    , ( "Cache-Control", Json.Encode.string "max-age=0" )
                    ]
                }
            )

        -- TODO: possibly a (POST, _, Login) to "Set-Cookie"
        ( _, _, _ ) ->
            ( serverState
            , writeResponse request { statusCode = StatusNotFound, body = "Not found?", headers = [] }
            )



-- MsgFromClient update function


updateFromClient : Types.RequestContext -> Time.Posix -> Types.MsgFromClient -> ServerState -> ( ServerState, Task String MsgFromServer )
updateFromClient ctx now clientMsg serverState =
    case clientMsg of
        Types.SetGreeting s ->
            ( { serverState | greeting = s }
            , Task.succeed (Types.CurrentGreeting ("You said: <" ++ s ++ "> at " ++ Debug.toString now))
            )



--


headerDecoder : Json.Decode.Decoder Types.RequestContext
headerDecoder =
    Json.Decode.oneOf
        [ Json.Decode.map Types.Cookied (Json.Decode.field "cookie" Json.Decode.string)
        , Json.Decode.succeed Types.Anonymous
        ]


spaHtml =
    """
    <!DOCTYPE HTML>
    <html>
    <head>
      <meta charset="UTF-8">
      <title>create-elm-server</title>
      <script src="/assets/client.js?JS_SHA"></script>
      <meta name="viewport" content="width=device-width, initial-scale=1, maximum-scale=1, user-scalable=no">
    </head>
    <body>
    <div id="elm"></div>
    <script>
    var now = new Date()
    var app = Elm.Client.init({
      node: document.getElementById('elm'),
      flags: {}
    });

    if (window.WebSocket && app.ports && app.ports.websocketOut) {
      console.log('[js websocket]', window.WebSocket)
      ;(function (app, WebSocket) {
        var ws = {}
        app.ports.websocketOut.subscribe(function (msg) {
          try {
            console.log('[js websocket] send', msg)
            ws.conn.send(msg)
          } catch (e) {
            console.log('[js websocket] send fail', e) // e.g. ws.conn not established
          }
        })
        function connectWebSocket (app, wsUrl, optionalProtocol) {
          ws.conn = new WebSocket(wsUrl, optionalProtocol)
          ws.conn.onopen = function (event) {
            console.log('[js websocket] connected', event)
            app.ports.websocketConnected.send(event.timeStamp | 0)
          }
          ws.conn.onmessage = function (event) {
            console.log('[js websocket] message', event)
            app.ports.websocketIn.send(event.data)
          }
          ws.conn.onerror = function (event) {
            console.log('[js websocket] error', event)
          }
          ws.conn.onclose = function (event) {
            console.log('[js websocket] close', event)
            ws.conn.onclose = null
            ws.conn = null
            setTimeout(function () {
              console.log('[js websocket] retrying...')
              connectWebSocket(app, wsUrl, optionalProtocol)
            }, 1000)
          }
        }
        connectWebSocket(app, (window.location.protocol === 'https:' ? 'wss' : 'ws') + '://' + window.location.host)
      })(app, window.WebSocket)
    }

    </script>
    </body>
    </html>
    """
