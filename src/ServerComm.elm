module ServerComm
    exposing
        ( Organisation
        , Replyer
        , Speaker
        , getOrganisations
        , getSpeakers
        , submitNewOrganisation
        )

import Http
import Json.Decode as Decode
import Json.Encode as Encode


baseUrl : String
baseUrl =
    "http://localhost:8080"


getOrganisations : Http.Request (List Organisation)
getOrganisations =
    Http.get (baseUrl ++ "/organisations") (Decode.list orgDecoder)


orgDecoder : Decode.Decoder Organisation
orgDecoder =
    Decode.map2 Organisation
        (Decode.field "name" Decode.string)
        (Decode.field "shortName" Decode.string)


replyerDecoder : Decode.Decoder Replyer
replyerDecoder =
    Decode.map3 Replyer
        (Decode.field "_id" Decode.int)
        (Decode.field "name" Decode.string)
        (Decode.field "group" Decode.string)


getSpeakers : Organisation -> Http.Request (List Speaker)
getSpeakers organisation =
    Http.request
        { method = "GET"
        , headers = [ Http.header "X-organisation" organisation.shortName ]
        , url = baseUrl ++ "/speakerList"
        , body = Http.emptyBody
        , expect = Http.expectJson (Decode.list speakerDecoder)
        , timeout = Nothing
        , withCredentials = False
        }


speakerDecoder : Decode.Decoder Speaker
speakerDecoder =
    Decode.map7 Speaker
        (Decode.field "_id" Decode.int)
        (Decode.field "group" Decode.string)
        (Decode.field "name" Decode.string)
        (Decode.field "number" Decode.int)
        (Decode.field "replies" (Decode.list replyerDecoder))
        (Decode.field "sex" Decode.string)
        (Decode.field "speaking" Decode.bool)


type alias Organisation =
    { name : String
    , shortName : String
    }


type alias Replyer =
    { id : Int
    , name : String
    , group : String
    }


type alias Speaker =
    { id : Int
    , group : String
    , name : String
    , number : Int
    , replies : List Replyer
    , sex : String
    , speaking : Bool
    }


submitNewOrganisation : { name : String, shortName : String } -> Http.Request Organisation
submitNewOrganisation data =
    let
        orgEncoder =
            Encode.object
                [ ( "name", Encode.string <| data.name )
                , ( "shortName", Encode.string <| data.shortName )
                ]
    in
    Http.post (baseUrl ++ "/organisations") (Http.jsonBody orgEncoder) orgDecoder
