module Page.Settings exposing (Model, Msg, init, toSession, update, view)

{-| Page editing the settings, and exporting or importing them as a JSON file.
-}

import File exposing (File)
import File.Select
import Html exposing (Html, button, div, h1, h2, p, text)
import Html.Attributes exposing (class, type_)
import Html.Events exposing (onClick)
import Session exposing (Session)
import Settings exposing (Settings)
import Settings.Persisted as Persisted
import Settings.Storage as Storage
import Task
import Ui.BoundedIntInput as BoundedIntInput
import Ui.Button as Button
import Ui.Card as Card
import Ui.DownloadLink as DownloadLink
import Ui.Notice as Notice
import Ui.SettingRow as SettingRow
import Ui.Toggle as Toggle


type alias Model =
    { session : Session
    , importStatus : Maybe ImportStatus
    }


type ImportStatus
    = Imported
    | Rejected


type Msg
    = ToggledSoundEffects Bool
    | ToggledAutoNextRound Bool
    | ChangedPreRoundDelay Int
    | ClickedImport
    | SelectedImportFile File
    | ReadImportFile String
    | DismissedImportStatus


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session, importStatus = Nothing }, Cmd.none )


toSession : Model -> Session
toSession model =
    model.session


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggledSoundEffects isEnabled ->
            applySettings (Settings.setSoundEffects isEnabled (settings model)) model

        ToggledAutoNextRound isEnabled ->
            applySettings (Settings.setAutoNextRound isEnabled (settings model)) model

        ChangedPreRoundDelay seconds ->
            applySettings (Settings.setPreRoundDelay seconds (settings model)) model

        ClickedImport ->
            ( model, File.Select.file [ Persisted.mimeType ] SelectedImportFile )

        SelectedImportFile file ->
            ( model, Task.perform ReadImportFile (File.toString file) )

        ReadImportFile content ->
            importSettings content model

        DismissedImportStatus ->
            ( { model | importStatus = Nothing }, Cmd.none )


{-| Replaces the settings when the file is valid, keeps the current ones
otherwise.
-}
importSettings : String -> Model -> ( Model, Cmd Msg )
importSettings content model =
    case Persisted.decodeString content of
        Ok (Persisted.UpToDate imported) ->
            applySettings imported { model | importStatus = Just Imported }

        Ok (Persisted.Migrated imported) ->
            applySettings imported { model | importStatus = Just Imported }

        Err _ ->
            ( { model | importStatus = Just Rejected }, Cmd.none )


applySettings : Settings -> Model -> ( Model, Cmd Msg )
applySettings newSettings model =
    ( { model | session = Session.putSettings newSettings model.session }, Storage.save newSettings )


settings : Model -> Settings
settings model =
    Session.settings model.session


view : Model -> { content : Html Msg, title : String }
view model =
    { content = viewContent model
    , title = "Settings"
    }


viewContent : Model -> Html Msg
viewContent model =
    div [ class "bg-gray-50 px-4 py-6 sm:py-10" ]
        [ div [ class "mx-auto flex max-w-2xl flex-col gap-6" ]
            [ h1 [ class "text-2xl font-bold tracking-tight text-gray-800" ] [ text "Settings" ]
            , viewImportStatus model.importStatus
            , viewSettings (settings model)
            , viewBackup (settings model)
            ]
        ]


viewSettings : Settings -> Html Msg
viewSettings currentSettings =
    div [ class (Card.classes ++ " divide-y divide-gray-100") ]
        [ SettingRow.view
            { title = "Enable sound effects", description = "Play beeps when a step starts and during its last seconds." }
            (Toggle.view { label = "Enable sound effects", isOn = Settings.soundEffects currentSettings, onToggle = ToggledSoundEffects })
        , SettingRow.view
            { title = "Move to next round automatically", description = "When disabled, press Start to begin each round." }
            (Toggle.view { label = "Move to next round automatically", isOn = Settings.autoNextRound currentSettings, onToggle = ToggledAutoNextRound })
        , SettingRow.view
            { title = "Extra pre-round delay", description = "Seconds added to the countdown starting each round." }
            (BoundedIntInput.view { label = "Extra pre-round delay", unit = "s", value = Settings.preRoundDelay currentSettings, onChange = ChangedPreRoundDelay })
        ]


viewBackup : Settings -> Html Msg
viewBackup currentSettings =
    div [ class (Card.classes ++ " px-6 py-4") ]
        [ h2 [ class "font-medium text-gray-800" ] [ text "Backup" ]
        , p [ class "mb-4 text-sm text-gray-500" ] [ text "Export your settings to a file to restore them later or on another device." ]
        , div [ class "flex gap-3" ]
            [ DownloadLink.view
                { fileName = Persisted.exportFileName, mimeType = Persisted.mimeType, content = Persisted.toString currentSettings }
                [ class (Button.primaryClasses ++ " px-6 py-2") ]
                [ text "Export" ]
            , button [ type_ "button", onClick ClickedImport, class (Button.secondaryClasses ++ " px-6 py-2") ] [ text "Import" ]
            ]
        ]


viewImportStatus : Maybe ImportStatus -> Html Msg
viewImportStatus maybeImportStatus =
    case maybeImportStatus of
        Just Imported ->
            Notice.view { tone = Notice.Info, content = [ text "Settings imported." ], onDismiss = DismissedImportStatus }

        Just Rejected ->
            Notice.view { tone = Notice.Error, content = [ text "This settings file isn't valid, your current settings were kept." ], onDismiss = DismissedImportStatus }

        Nothing ->
            text ""
