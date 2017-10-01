module Http.Parser exposing (request, andThen, json, Request, Error(..))

{-| This library provides methods to parse HTTP/1.1 requests string.

    -- Example usage parsing a json request
    let
        decoder =
            Decode.succeed ReqBody
                |: field "state" Decode.string
    in
        request message |> andThen (json decoder)


# Definition

@docs Request


# Parsing messages

@docs request


# Parsing JSON

@docs json


# Common helpers

@docs andThen


# Parser errors

@docs Error

-}

import Json.Decode as Decode
import Parser
    exposing
        ( Parser
        , Count(..)
        , (|.)
        , (|=)
        , succeed
        , ignore
        , oneOf
        , keyword
        , source
        , ignoreUntil
        , keep
        , zeroOrMore
        , symbol
        , andThen
        , fail
        , end
        , delayedCommit
        )
import Dict exposing (Dict)


type alias Method =
    String


type alias RequestUri =
    String


type alias HttpVersion =
    String


type alias PartialRequest1 =
    { requestLine : ( Method, RequestUri, HttpVersion )
    , headers : Dict String String
    }


{-| Represent a parsed request with a parameterized body type
-}
type alias Request body =
    { method : String
    , uri : String
    , headers : Dict String String
    , body : body
    }


oneSpace =
    ignore (Exactly 1) (\c -> c == ' ')



-- path =
--     source <|
--         ignore (Parser.Exactly 1) (\c -> c == '/')
--             |. ignore zeroOrMore (\c -> c /= ' ' && c /= '?')
-- query =
--     source <|
--         ignore (Parser.Exactly 1) (\c -> c == '?')
--             |. ignore zeroOrMore (\c -> c /= ' ')


method =
    source <|
        oneOf
            [ keyword "OPTIONS"
            , keyword "GET"
            , keyword "HEAD"
            , keyword "POST"
            , keyword "PUT"
            , keyword "DELETE"
            , keyword "TRACE"
            , keyword "CONNECT"
            ]


isNotWhitespace c =
    c /= ' '


uri =
    source <|
        ignore (AtLeast 1) isNotWhitespace


version =
    source <| oneOf [ keyword "HTTP/1.1" ]


isHeaderNameChar c =
    (c /= '\x0D' && c /= '\n' && c /= ' ' && c /= ':')


headerName =
    source <|
        ignore (AtLeast 1) isHeaderNameChar


isHeaderValueChar c =
    (c /= ' ' && c /= '\x0D' && c /= '\n')


headerValue =
    source <|
        ignore (AtLeast 1) isHeaderValueChar


colon =
    symbol ":"


newline =
    oneOf
        [ symbol "\n"
        , symbol "\x0D\n"
        ]


header =
    succeed (,)
        |= headerName
        |. colon
        |. oneSpace
        |= headerValue
        |. newline


headersHelp revHeaders =
    oneOf
        [ header
            |> Parser.andThen (\( k, v ) -> headersHelp (Dict.insert k v revHeaders))
        , succeed revHeaders
        ]


headers =
    oneOf
        [ Parser.andThen (\( k, v ) -> headersHelp <| Dict.singleton k v) header
        , succeed Dict.empty
        ]


requestLine =
    succeed (,,)
        |= method
        |. oneSpace
        |= uri
        |. oneSpace
        |= version
        |. newline


partial1 =
    succeed PartialRequest1
        |= requestLine
        |= headers


entityBody length =
    case length of
        0 ->
            succeed ""

        n ->
            delayedCommit newline <|
                succeed identity
                    |= keep (Exactly n) (always True)


parseMessageBody partial1 =
    let
        contentLength =
            Dict.get "Content-Length" partial1.headers
                |> Maybe.map String.toInt
                |> Maybe.withDefault (Ok 0)
    in
        case contentLength of
            Ok length ->
                let
                    ( method, uri, version ) =
                        partial1.requestLine
                in
                    succeed Request
                        |= succeed method
                        |= succeed uri
                        |= succeed partial1.headers
                        |= entityBody length
                        |. end

            Err err ->
                fail err


requestParser : Parser (Request String)
requestParser =
    Parser.andThen parseMessageBody partial1


{-| Represent an unexpected error while trying to parse or decode some part of the message
-}
type Error
    = ParserError Parser.Error
    | DecoderError String


{-| Parse a request message into a request representation
-}
request : String -> Result Error (Request String)
request string =
    Parser.run requestParser string
        |> Result.mapError ParserError


addBody req body =
    { req | body = body }


{-| Decode the body of some previously parsed request expecting a json string and a Content-Type header with value "application/json"
-}
json : Decode.Decoder a -> Request String -> Result Error (Request a)
json bodyDecoder req =
    let
        ctype =
            Dict.get "Content-Type" req.headers
    in
        case ctype of
            Just "application/json" ->
                Decode.decodeString bodyDecoder req.body
                    |> Result.mapError DecoderError
                    |> Result.map (addBody req)

            Just bad ->
                Err <|
                    DecoderError <|
                        "BadContentType: I'm expecting \"application/json\" but instead got \""
                            ++ bad
                            ++ "\""

            Nothing ->
                Err <| DecoderError "MissingContentTypeHeader"


{-| Chain a request parser to some body decoder
-}
andThen : (Request String -> Result Error (Request a)) -> Result Error (Request String) -> Result Error (Request a)
andThen =
    Result.andThen
