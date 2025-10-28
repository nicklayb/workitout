module Main exposing (..)

import AppConfig exposing (AppConfig)
import Browser exposing (Document)
import Browser.Navigation as Nav
import Html exposing (Html)
import Page
import Page.Blank as BlankPage
import Page.Errors.NotFound as NotFoundPage
import Page.Home as HomePage
import Page.RunPlan as RunPlanPage
import Route exposing (Route(..))
import Session exposing (Session)
import Url exposing (Url)


type Model
    = Redirect Session
    | NotFound Session
    | Home HomePage.Model
    | RunPlan RunPlanPage.Model


type Msg
    = ChangedRoute (Maybe Route)
    | ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | GotHomeMsg HomePage.Msg
    | GotRunPlanMsg RunPlanPage.Msg


main : Program AppConfig Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        _ ->
            Sub.none


init : AppConfig -> Url -> Nav.Key -> ( Model, Cmd Msg )
init appConfig url navKey =
    changeRouteTo (Route.fromUrl url)
        (Redirect (Session.init navKey url appConfig))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( ClickedLink urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    let
                        session =
                            toSession model
                    in
                    if Session.urlChanged url session then
                        let
                            newSession =
                                Session.putUrl url
                        in
                        ( putSession (newSession session) model, Nav.pushUrl (Session.navKey session) (Url.toString url) )

                    else
                        ( model, Cmd.none )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        ( ChangedUrl url, _ ) ->
            changeRouteTo (Route.fromUrl url) model

        ( ChangedRoute route, _ ) ->
            changeRouteTo route model

        ( GotHomeMsg subMsg, Home home ) ->
            HomePage.update subMsg home
                |> updateWith Home GotHomeMsg model

        ( GotRunPlanMsg subMsg, RunPlan home ) ->
            RunPlanPage.update subMsg home
                |> updateWith RunPlan GotRunPlanMsg model

        ( _, _ ) ->
            ( model, Cmd.none )


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        session =
            toSession model
    in
    case maybeRoute of
        Just Route.Home ->
            HomePage.init session
                |> updateWith Home GotHomeMsg model

        Just (Route.RunPlan maybeString maybeDay) ->
            RunPlanPage.init maybeString maybeDay session
                |> updateWith RunPlan GotRunPlanMsg model

        Nothing ->
            ( NotFound session, Cmd.none )


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg _ ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )


toSession : Model -> Session
toSession page =
    case page of
        Redirect session ->
            session

        NotFound session ->
            session

        Home home ->
            HomePage.toSession home

        RunPlan home ->
            RunPlanPage.toSession home


putSession : Session -> Model -> Model
putSession session page =
    case page of
        Redirect _ ->
            Redirect session

        NotFound _ ->
            NotFound session

        Home home ->
            Home (Session.putSession session home)

        RunPlan runPlan ->
            RunPlan (Session.putSession session runPlan)


view : Model -> Document Msg
view model =
    let
        session =
            toSession model

        viewPage page toMsg config =
            let
                { title, body } =
                    Page.view session page config
            in
            { title = title
            , body = List.map (Html.map toMsg) body
            }
    in
    case model of
        Redirect _ ->
            Page.view session Page.Other BlankPage.view

        Home home ->
            viewPage Page.Home GotHomeMsg (HomePage.view home)

        RunPlan runPlan ->
            viewPage Page.RunPlan GotRunPlanMsg (RunPlanPage.view runPlan)

        _ ->
            Page.view session Page.Other NotFoundPage.view
