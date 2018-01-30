module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import ServerComm exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


getOrganisations : Cmd Msg
getOrganisations =
    ServerComm.getOrganisations
        |> Http.send OrganisationResult


type alias Model =
    { organisations : List Organisation
    , newOrgName : String
    , newOrgShortName : String
    }


initialModel : Model
initialModel =
    { organisations = [], newOrgName = "", newOrgShortName = "" }


init : ( Model, Cmd Msg )
init =
    ( initialModel, getOrganisations )



-- UPDATE


type Msg
    = OrganisationResult (Result Http.Error (List Organisation))
    | OrgNameInput String
    | ShortNameInput String
    | SubmitNewOrganisation
    | SubmitResult (Result Http.Error Organisation)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OrganisationResult (Err _) ->
            model ! []

        OrganisationResult (Ok orgs) ->
            { model | organisations = orgs } ! []

        OrgNameInput name ->
            { model | newOrgName = name } ! []

        ShortNameInput shortName ->
            { model | newOrgShortName = shortName } ! []

        SubmitNewOrganisation ->
            { model | newOrgName = "", newOrgShortName = "" } ! [ submitNewOrganisation model ]

        SubmitResult (Err _) ->
            model ! []

        SubmitResult (Ok org) ->
            model ! [ getOrganisations ]


submitNewOrganisation : Model -> Cmd Msg
submitNewOrganisation model =
    ServerComm.submitNewOrganisation { name = model.newOrgName, shortName = model.newOrgShortName }
        |> Http.send SubmitResult



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Ro i salen" ]
        , viewOrganisations model.organisations
        , newOrganisation model
        , footer
        ]


newOrganisation : Model -> Html Msg
newOrganisation model =
    div []
        [ div []
            [ input
                [ onInput OrgNameInput
                , placeholder "Navn på organisasjon"
                , value model.newOrgName
                ]
                []
            ]
        , div []
            [ input
                [ onInput ShortNameInput
                , placeholder "Kortnavn uten mellomrom og andre spesialteikn"
                , value model.newOrgShortName
                ]
                []
            ]
        , div [] [ button [ onClick SubmitNewOrganisation ] [ text "Legg til" ] ]
        ]


viewOrganisations : List Organisation -> Html Msg
viewOrganisations orgs =
    let
        view org =
            p [] [ text org.name ]
    in
    orgs
        |> List.map view
        |> div []


footer : Html m
footer =
    div []
        [ h2 [] [ text "Om Roisalen" ]
        , p [] [ text "Vi prøver å lage enkle digitale verktøy for å gjøre store møter bedre. Foreløpig har vi laget verktøy som:" ]
        , ul []
            [ li [] [ text "Holder styr på talelista for deg" ]
            , li [] [ text "Innebygd klokke for å sørge for at alle får prate like lenge" ]
            , li [] [ text "Lager statistikk over hvem som har pratet mest på dette møtet" ]
            , li [] [ text "Viser sakstittel og dele beskjeder med salen" ]
            ]
        , p []
            [ text """
                Roisalen er laget på fritida av tre aktive organisasjonsmennesker:
                Christian Strandenæs, Stian Lågstad og Torkil Vederhus.
                All kildekode er åpent tilgjengelig på """
            , a [ href "" ] [ text "GitHub" ]
            , text """
                , bidra gjerne selv!
                  Ønsker om funksjonalitet og spesialtilpasning kan sendes til torkilv(a)gmail.com"""
            ]
        ]
