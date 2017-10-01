# Http.Parser

TODO:

- [ ] RFC2616

This library provides methods to parse HTTP/1.1 requests string.

    -- Example usage parsing a json request
    let
        decoder =
            Decode.succeed ReqBody
                |: field "state" Decode.string
    in
        request message |> andThen (json decoder)

# Definition

    type alias Request body = 
        { method : String
        , uri : String
        , headers : Dict String String
        , body : body
        }

Represent a parsed request with a parameterized body type

# Parsing messages

    request : String -> Result Error (Request String)

Parse a request message into a request representation

# Parsing JSON

    json : Decoder a -> Request String -> Result Error (Request a)
    
Decode the body of some previously parsed request expecting a json string and a `Content-Type` header with value `application/json`

# Common helpers
    
    andThen
        :  (Request String -> Result Error (Request a))
        -> Result Error (Request String)
        -> Result Error (Request a)

Chain a request parser to some body decoder

# Parser errors
  
    type Error
        = ParserError Error
        | DecoderError String

Represent an unexpected error while trying to parse or decode some part of the message
