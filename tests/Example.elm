module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Http.Parser exposing (request, andThen, json)
import Json.Decode as Decode exposing (field)
import Json.Decode.Extra exposing ((|:))
import Dict


type alias ReqBody =
    { state : String
    }


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


noHeaders =
    ( "Handle requests without headers"
    , "POST /api?hola=mundo%20foo%20bar HTTP/1.1\x0D\n"
    , Ok
        { method = "POST"
        , uri = "/api?hola=mundo%20foo%20bar"
        , headers = Dict.fromList []
        , body = ""
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


suite : Test
suite =
    describe "Http.Parser"
        [ describe "request"
            [ let
                ( feature, sample, expected ) =
                    good1
              in
                test feature (\_ -> Expect.equal (request sample) expected)
            , let
                ( feature, sample, expected ) =
                    noHeaders
              in
                test feature (\_ -> Expect.equal (request sample) expected)
            ]
        , describe "body"
            [ let
                ( feature, sample, expected ) =
                    jsonBody1

                decoder =
                    Decode.succeed ReqBody
                        |: field "state" Decode.string
              in
                test feature (\_ -> Expect.equal (request sample |> andThen (json decoder)) expected)
            ]
        ]
