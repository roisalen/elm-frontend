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


getSpeakers : Organisation -> Cmd Msg
getSpeakers organisation =
    ServerComm.getSpeakers organisation
        |> Http.send (SpeakersResult organisation)


type alias Model =
    { organisations : List Organisation
    , speakers : List Speaker
    , chosenOrganisation : Maybe Organisation
    , newOrgName : String
    , newOrgShortName : String
    , page : Page
    }


initialModel : Model
initialModel =
    { organisations = []
    , speakers = []
    , chosenOrganisation = Nothing
    , newOrgName = ""
    , newOrgShortName = ""
    , page = LandingPage
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, getOrganisations )


type Msg
    = OrganisationResult (Result Http.Error (List Organisation))
    | SpeakersResult Organisation (Result Http.Error (List Speaker))
    | OrgNameInput String
    | ShortNameInput String
    | SubmitNewOrganisation
    | SubmitResult (Result Http.Error Organisation)
    | ChooseOrganisation Organisation
    | SelectPage Page


type Page
    = LandingPage
    | Speakerlist
    | AdminRepresentants
    | LeadMeeting
    | Statistics


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OrganisationResult (Err _) ->
            model ! []

        OrganisationResult (Ok orgs) ->
            { model | organisations = orgs } ! []

        SpeakersResult organisation (Err _) ->
            model ! []

        SpeakersResult organisation (Ok speakers) ->
            { model | speakers = speakers } ! []

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

        ChooseOrganisation organisation ->
            { model | chosenOrganisation = Just organisation, page = Speakerlist } ! []

        SelectPage page ->
            let
                c =
                    case page of
                        LeadMeeting ->
                            model.chosenOrganisation
                                |> Maybe.map getSpeakers
                                |> Maybe.withDefault Cmd.none

                        _ ->
                            Cmd.none
            in
            { model | page = page } ! [ c ]


submitNewOrganisation : Model -> Cmd Msg
submitNewOrganisation model =
    ServerComm.submitNewOrganisation { name = model.newOrgName, shortName = model.newOrgShortName }
        |> Http.send SubmitResult


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    let
        title =
            model.chosenOrganisation
                |> Maybe.map (\o -> " – " ++ o.name)
                |> Maybe.withDefault ""
    in
    div []
        [ h1 [] [ "Ro i salen" ++ title |> text ]
        , navigationBar model.page
        , case model.page of
            LandingPage ->
                div []
                    [ viewOrganisations model.organisations
                    , newOrganisation model
                    , footer
                    ]

            Speakerlist ->
                h2 [] [ text "Taleliste" ]

            AdminRepresentants ->
                h2 [] [ text "Registrerte representanter" ]

            LeadMeeting ->
                viewLeadMeeting model

            Statistics ->
                h2 [] [ text "Dagens statistikk" ]
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
            p [] [ button [ onClick (ChooseOrganisation org) ] [ text org.name ] ]
    in
    orgs
        |> List.map view
        |> div []


navigationBar : Page -> Html Msg
navigationBar selectedPage =
    div []
        [ button [ onClick (SelectPage Speakerlist) ] [ text "Taleliste" ]
        , button [ onClick (SelectPage AdminRepresentants) ] [ text "Administrér representanter" ]
        , button [ onClick (SelectPage LeadMeeting) ] [ text "Styr ordet" ]
        , button [ onClick (SelectPage Statistics) ] [ text "Dagens statistikk" ]
        ]


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
                Christian Strandenæs, Stian Lågstad, Runar Furenes og Torkil Vederhus.
                All kildekode er åpent tilgjengelig på """
            , a [ href "" ] [ text "GitHub" ]
            , text """, bidra gjerne selv!
                  Ønsker om funksjonalitet og spesialtilpasning kan sendes til torkilv(a)gmail.com"""
            ]
        ]


viewLeadMeeting : Model -> Html Msg
viewLeadMeeting model =
    div []
        [ div [] [ label [] [ text "Sakstittel" ], input [ placeholder "Skriv inn sakstittel…" ] [] ]
        , div [] [ label [] [ text "Legg til / Neste taler" ], input [ placeholder "Ny taler: Skriv talenummer. Replikk: Skriv r+talenummer. Neste taler/replikk: Trykk enter i tomt felt" ] [] ]
        , div [] [ text "TODO Add timer here" ]
        , div [] [ label [] [ text "Beskjeder og info" ], textarea [] [] ]
        , div [] [ viewSpeakers model.speakers ]
        , viewRepresentants model
        ]


viewSpeakers : List Speaker -> Html Msg
viewSpeakers speakers =
    speakers
        |> List.concatMap viewSpeaker
        |> table []


viewSpeaker : Speaker -> List (Html Msg)
viewSpeaker speaker =
    let
        row id name group isReply =
            tr []
                [ td []
                    [ (if isReply then
                        "->"
                       else
                        ""
                      )
                        ++ (id |> toString)
                        |> text
                    ]
                , td [] [ text name ]
                , td [] [ text group ]
                ]
    in
    row speaker.id speaker.name speaker.group False
        :: (speaker.replies
                |> List.map (\r -> row r.id r.name r.group True)
           )


viewRepresentants : Model -> Html Msg
viewRepresentants model =
    h2 [] [ text "Registrerte representanter" ]
