module ServerComm exposing (Organisation, getOrganisations, submitNewOrganisation)

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


type alias Organisation =
    { name : String
    , shortName : String
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
