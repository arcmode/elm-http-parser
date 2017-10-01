module Http.Parser exposing (request, andThen, json, Error(..))

{-| This library provides a middleware abstraction that can be used to
run tasks in a pre-defined sequence.

@docs request, Error

-}

import Json.Decode as Decode
import Parser
    exposing
        ( Parser
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
        )
import Dict exposing (Dict)


exactly =
    Parser.Exactly


atLeast =
    Parser.AtLeast


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


type alias Request body =
    { method : String
    , uri : String
    , headers : Dict String String
    , body : body
    }


oneSpace =
    ignore (Parser.Exactly 1) (\c -> c == ' ')



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
        ignore (atLeast 1) isNotWhitespace


version =
    source <| oneOf [ keyword "HTTP/1.1" ]


isHeaderNameChar c =
    (c /= '\x0D' && c /= '\n' && c /= ' ' && c /= ':')


headerName =
    source <|
        ignore (atLeast 1) isHeaderNameChar


isHeaderValueChar c =
    (c /= ' ' && c /= '\x0D' && c /= '\n')


headerValue =
    source <|
        ignore (atLeast 1) isHeaderValueChar


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
    Parser.andThen (\( k, v ) -> headersHelp <| Dict.singleton k v) header


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
        |. newline


parseMessageBody partial1 =
    let
        contentLength =
            Dict.get "Content-Length" partial1.headers
                |> Result.fromMaybe "MissingContentLengthHeader"
                |> Result.andThen String.toInt
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
                        |= keep (exactly length) (always True)
                        |. end

            Err err ->
                fail err


requestParser : Parser (Request String)
requestParser =
    Parser.andThen parseMessageBody partial1


{-| Docs
-}
type Error
    = ParserError Parser.Error
    | DecoderError String


{-| Docs
-}
request : String -> Result Error (Request String)
request string =
    Parser.run requestParser string
        |> Result.mapError ParserError


addBody req body =
    { req | body = body }


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


andThen : (Request String -> Result Error (Request a)) -> Result Error (Request String) -> Result Error (Request a)
andThen =
    Result.andThen
