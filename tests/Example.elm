module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Http.Parser exposing (request, andThen, json)
import Samples
import Json.Decode as Decode exposing (field)
import Json.Decode.Extra exposing ((|:))


type alias ReqBody =
    { state : String
    }


suite : Test
suite =
    describe "Http.Parser"
        [ describe "request"
            [ let
                ( feature, sample, expected ) =
                    Samples.good1
              in
                test feature (\_ -> Expect.equal (request sample) expected)
            ]
        , describe "body"
            [ let
                ( feature, sample, expected ) =
                    Samples.jsonBody1

                decoder =
                    Decode.succeed ReqBody
                        |: field "state" Decode.string
              in
                test feature (\_ -> Expect.equal (request sample |> andThen (json decoder)) expected)
            ]
        ]
