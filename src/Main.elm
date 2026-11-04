module Main exposing (..)

import AppConfig exposing (AppConfig)
import Browser exposing (Document)
import Browser.Navigation as Nav
import CustomPlans
import CustomPlans.Storage as CustomPlansStorage
import Html exposing (Html, text)
import Html.Attributes
import Page
import Page.Blank as BlankPage
import Page.Errors.NotFound as NotFoundPage
import Page.Home as HomePage
import Page.PlanEditor as PlanEditorPage
import Page.RunPlan as RunPlanPage
import Page.Settings as SettingsPage
import Route exposing (Route(..))
import Session exposing (Session)
import Settings.Persisted as Persisted
import Settings.Storage as Storage
import Ui.DownloadLink as DownloadLink
import Ui.Notice as Notice
import Url exposing (Url)


type Model
    = Redirect Session
    | NotFound Session
    | Home HomePage.Model
    | RunPlan RunPlanPage.Model
    | Settings SettingsPage.Model
    | PlanEditor PlanEditorPage.Model


type Msg
    = ChangedRoute (Maybe Route)
    | ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | GotHomeMsg HomePage.Msg
    | GotRunPlanMsg RunPlanPage.Msg
    | GotSettingsMsg SettingsPage.Msg
    | GotPlanEditorMsg PlanEditorPage.Msg
    | DismissedNotice Int


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
    let
        ( sessionWithSettings, settingsStorageCmd ) =
            loadStoredSettings appConfig.settings (Session.init navKey url appConfig)

        ( session, plansStorageCmd ) =
            loadStoredPlans appConfig.customPlans sessionWithSettings

        ( model, routeCmd ) =
            changeRouteTo (Route.fromUrl url) (Redirect session)
    in
    ( model, Cmd.batch [ settingsStorageCmd, plansStorageCmd, routeCmd ] )


{-| Restores the settings saved in the local storage. Outdated ones are
migrated and saved back, invalid ones are removed; both cases notify the user.
-}
loadStoredSettings : Maybe String -> Session -> ( Session, Cmd Msg )
loadStoredSettings maybeStoredSettings session =
    case Maybe.map Persisted.decodeString maybeStoredSettings of
        Nothing ->
            ( session, Cmd.none )

        Just (Ok (Persisted.UpToDate settings)) ->
            ( Session.putSettings settings session, Cmd.none )

        Just (Ok (Persisted.Migrated settings)) ->
            ( session
                |> Session.putSettings settings
                |> Session.addNotice Session.SettingsMigrated
            , Storage.save settings
            )

        Just (Err _) ->
            ( Session.addNotice Session.SavedSettingsInvalid session, Storage.clear )


{-| Restores the user's plans saved in the local storage. Invalid ones are
removed and the user is notified, with a way to download what was removed.
-}
loadStoredPlans : Maybe String -> Session -> ( Session, Cmd Msg )
loadStoredPlans maybeStoredPlans session =
    case maybeStoredPlans of
        Nothing ->
            ( session, Cmd.none )

        Just storedPlans ->
            case CustomPlans.decodeString storedPlans of
                Ok customPlans ->
                    ( Session.putCustomPlans customPlans session, Cmd.none )

                Err _ ->
                    ( Session.addNotice (Session.SavedPlansInvalid storedPlans) session, CustomPlansStorage.clear )


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

        ( GotSettingsMsg subMsg, Settings settings ) ->
            SettingsPage.update subMsg settings
                |> updateWith Settings GotSettingsMsg model

        ( GotPlanEditorMsg subMsg, PlanEditor planEditor ) ->
            PlanEditorPage.update subMsg planEditor
                |> updateWith PlanEditor GotPlanEditorMsg model

        ( DismissedNotice index, _ ) ->
            ( putSession (Session.dismissNotice index (toSession model)) model, Cmd.none )

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

        Just (Route.RunPlan source maybeDay) ->
            RunPlanPage.init source maybeDay session
                |> updateWith RunPlan GotRunPlanMsg model

        Just Route.Settings ->
            SettingsPage.init session
                |> updateWith Settings GotSettingsMsg model

        Just (Route.PlanEditor target) ->
            PlanEditorPage.init target session
                |> updateWith PlanEditor GotPlanEditorMsg model

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

        Settings settings ->
            SettingsPage.toSession settings

        PlanEditor planEditor ->
            PlanEditorPage.toSession planEditor


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

        Settings settings ->
            Settings (Session.putSession session settings)

        PlanEditor planEditor ->
            PlanEditor (Session.putSession session planEditor)


view : Model -> Document Msg
view model =
    let
        session =
            toSession model

        notices =
            List.indexedMap (viewNotice session) (Session.notices session)

        viewPage page toMsg { title, content } =
            Page.view session page notices { title = title, content = Html.map toMsg content }
    in
    case model of
        Redirect _ ->
            Page.view session Page.Other notices BlankPage.view

        Home home ->
            viewPage Page.Home GotHomeMsg (HomePage.view home)

        RunPlan runPlan ->
            viewPage Page.RunPlan GotRunPlanMsg (RunPlanPage.view runPlan)

        Settings settings ->
            viewPage Page.Settings GotSettingsMsg (SettingsPage.view settings)

        PlanEditor planEditor ->
            viewPage Page.PlanEditor GotPlanEditorMsg (PlanEditorPage.view planEditor)

        _ ->
            Page.view session Page.Other notices NotFoundPage.view


viewNotice : Session -> Int -> Session.Notice -> Html Msg
viewNotice session index notice =
    case notice of
        Session.SavedSettingsInvalid ->
            Notice.view
                { tone = Notice.Error
                , content = [ text "Saved settings aren't valid" ]
                , onDismiss = DismissedNotice index
                }

        Session.SettingsMigrated ->
            Notice.view
                { tone = Notice.Info
                , content =
                    [ text "Your settings have been updated automatically, make sure to download the updated copy "
                    , DownloadLink.view
                        { fileName = Persisted.exportFileName
                        , mimeType = Persisted.mimeType
                        , content = Persisted.toString (Session.settings session)
                        }
                        [ Html.Attributes.class "font-semibold underline" ]
                        [ text "here" ]
                    ]
                , onDismiss = DismissedNotice index
                }

        Session.SavedPlansInvalid storedPlans ->
            Notice.view
                { tone = Notice.Error
                , content =
                    [ text "Saved plans aren't valid and were removed, you can still download them "
                    , DownloadLink.view
                        { fileName = "workitout-invalid-plans.json"
                        , mimeType = Persisted.mimeType
                        , content = storedPlans
                        }
                        [ Html.Attributes.class "font-semibold underline" ]
                        [ text "here" ]
                    ]
                , onDismiss = DismissedNotice index
                }
