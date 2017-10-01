module Samples exposing (..)

import Dict


good1 =
    ( "Understands HTTP/1.1"
    , "POST /api?hola=mundo%20foo%20bar HTTP/1.1\x0D\nHost: localhost:8080\x0D\nUser-Agent: curl/7.54.0\x0D\nContent-Length: 13\x0D\nAccept: */*\x0D\nContent-Type: application/x-www-form-urlencoded\x0D\n\x0D\n{\"state\": \"\"}"
    , Ok
        { method = "POST"
        , uri = "/api?hola=mundo%20foo%20bar"
        , headers =
            Dict.fromList
                [ ( "Host", "localhost:8080" )
                , ( "User-Agent", "curl/7.54.0" )
                , ( "Content-Length", "13" )
                , ( "Accept", "*/*" )
                , ( "Content-Type", "application/x-www-form-urlencoded" )
                ]
        , body = "{\"state\": \"\"}"
        }
    )


jsonBody1 =
    ( "Understands JSON"
    , "POST /api?hola=mundo%20foo%20bar HTTP/1.1\x0D\nHost: localhost:8080\x0D\nUser-Agent: curl/7.54.0\x0D\nContent-Length: 13\x0D\nAccept: */*\x0D\nContent-Type: application/json\x0D\n\x0D\n{\"state\": \"\"}"
    , Ok
        { method = "POST"
        , uri = "/api?hola=mundo%20foo%20bar"
        , headers =
            Dict.fromList
                [ ( "Host", "localhost:8080" )
                , ( "User-Agent", "curl/7.54.0" )
                , ( "Content-Length", "13" )
                , ( "Accept", "*/*" )
                , ( "Content-Type", "application/json" )
                ]
        , body = { state = "" }
        }
    )
