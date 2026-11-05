module Page.Settings exposing (Model, Msg, init, toSession, update, view)

{-| Page editing the settings, and exporting or importing them as a JSON file.
-}

import Audius.Playlist as Playlist
import File exposing (File)
import File.Select
import Html exposing (Html, a, button, div, h1, h2, p, text)
import Html.Attributes exposing (class, href, rel, target, type_)
import Html.Events exposing (onClick)
import Session exposing (Session)
import Settings exposing (BreakMusic(..), EffortMusic(..), Settings)
import Settings.Persisted as Persisted
import Settings.Storage as Storage
import Task
import Ui.BoundedIntInput as BoundedIntInput
import Ui.Button as Button
import Ui.Card as Card
import Ui.DownloadLink as DownloadLink
import Ui.Notice as Notice
import Ui.Select as Select
import Ui.SettingRow as SettingRow
import Ui.TextInput as TextInput
import Ui.Toggle as Toggle


type alias Model =
    { session : Session
    , importStatus : Maybe ImportStatus
    , effortPlaylistText : String
    , breakPlaylistText : String
    }


type ImportStatus
    = Imported
    | Rejected


type Msg
    = ToggledSoundEffects Bool
    | ToggledAutoNextRound Bool
    | ChangedPreRoundDelay Int
    | SelectedEffortMusic EffortMusic
    | ChangedEffortPlaylist String
    | SelectedBreakMusic BreakMusic
    | ChangedBreakPlaylist String
    | ChangedDimmedEffortVolume Int
    | ClickedImport
    | SelectedImportFile File
    | ReadImportFile String
    | DismissedImportStatus


init : Session -> ( Model, Cmd Msg )
init session =
    ( resetPlaylistTexts { session = session, importStatus = Nothing, effortPlaylistText = "", breakPlaylistText = "" }, Cmd.none )


{-| Fills the playlist fields with the playlists of the settings.
-}
resetPlaylistTexts : Model -> Model
resetPlaylistTexts model =
    { model
        | effortPlaylistText = Playlist.toUrl (Settings.effortPlaylist (settings model))
        , breakPlaylistText = Playlist.toUrl (Settings.breakPlaylist (settings model))
    }


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

        SelectedEffortMusic music ->
            applySettings (Settings.setEffortMusic music (settings model)) model

        ChangedEffortPlaylist url ->
            applyPlaylistText Settings.setEffortPlaylist url { model | effortPlaylistText = url }

        SelectedBreakMusic music ->
            applySettings (Settings.setBreakMusic music (settings model)) model

        ChangedBreakPlaylist url ->
            applyPlaylistText Settings.setBreakPlaylist url { model | breakPlaylistText = url }

        ChangedDimmedEffortVolume percent ->
            applySettings (Settings.setDimmedEffortVolume percent (settings model)) model

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
            applyImportedSettings imported model

        Ok (Persisted.Migrated imported) ->
            applyImportedSettings imported model

        Err _ ->
            ( { model | importStatus = Just Rejected }, Cmd.none )


applyImportedSettings : Settings -> Model -> ( Model, Cmd Msg )
applyImportedSettings imported model =
    applySettings imported { model | importStatus = Just Imported }
        |> Tuple.mapFirst resetPlaylistTexts


{-| Saves the playlist of a playlist field when it holds a valid URL. Invalid
texts are kept in the field only.
-}
applyPlaylistText : (Playlist.Playlist -> Settings -> Settings) -> String -> Model -> ( Model, Cmd Msg )
applyPlaylistText setPlaylist url model =
    case Playlist.fromUrl url of
        Just playlist ->
            applySettings (setPlaylist playlist (settings model)) model

        Nothing ->
            ( model, Cmd.none )


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
            , viewMusic model
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


viewMusic : Model -> Html Msg
viewMusic model =
    let
        currentSettings =
            settings model

        effortPlaylistRow =
            if Settings.effortMusic currentSettings == EffortFromPlaylist then
                viewPlaylistField { label = "Effort playlist", url = model.effortPlaylistText, onInput = ChangedEffortPlaylist }

            else
                text ""

        breakDetailRow =
            case Settings.breakMusic currentSettings of
                BreakSilent ->
                    text ""

                BreakDimmedEffort ->
                    SettingRow.view
                        { title = "Dimmed volume", description = "Volume of the effort music during breaks." }
                        (BoundedIntInput.view { label = "Dimmed volume", unit = "%", value = Settings.dimmedEffortVolume currentSettings, onChange = ChangedDimmedEffortVolume })

                BreakFromPlaylist ->
                    viewPlaylistField { label = "Break playlist", url = model.breakPlaylistText, onInput = ChangedBreakPlaylist }
    in
    div [ class (Card.classes ++ " divide-y divide-gray-100") ]
        [ div [ class "px-6 py-4" ]
            [ h2 [ class "font-medium text-gray-800" ] [ text "Music" ]
            , p [ class "text-sm text-gray-500" ]
                [ text "Music plays in the background once you press Start. Paste the link of any public playlist from "
                , a [ href "https://audius.co", target "_blank", rel "noopener noreferrer", class "font-medium text-pink-500 hover:text-pink-600" ] [ text "Audius" ]
                , text "."
                ]
            ]
        , SettingRow.view
            { title = "Effort music", description = "Played during effort steps." }
            (div [ class "w-56" ]
                [ Select.view
                    { label = "Effort music"
                    , options = [ EffortSilent, EffortFromPlaylist ]
                    , selected = Settings.effortMusic currentSettings
                    , toKey = effortMusicKey
                    , toLabel = effortMusicLabel
                    , onSelect = SelectedEffortMusic
                    }
                ]
            )
        , effortPlaylistRow
        , SettingRow.view
            { title = "Break music", description = "Played during breaks." }
            (div [ class "w-56" ]
                [ Select.view
                    { label = "Break music"
                    , options = [ BreakSilent, BreakDimmedEffort, BreakFromPlaylist ]
                    , selected = Settings.breakMusic currentSettings
                    , toKey = breakMusicKey
                    , toLabel = breakMusicLabel
                    , onSelect = SelectedBreakMusic
                    }
                ]
            )
        , breakDetailRow
        ]


{-| Field holding the URL of a playlist, with an error while it isn't valid.
-}
viewPlaylistField : { label : String, url : String, onInput : String -> Msg } -> Html Msg
viewPlaylistField config =
    let
        error =
            if Playlist.fromUrl config.url == Nothing then
                p [ class "mt-1 text-sm text-red-600" ] [ text "Paste a link like https://audius.co/artist/playlist/name" ]

            else
                text ""
    in
    div [ class "px-6 py-4" ]
        [ TextInput.view { label = config.label, inputType = "url", placeholder = "https://audius.co/artist/playlist/name", value = config.url, onInput = config.onInput }
        , error
        ]


effortMusicKey : EffortMusic -> String
effortMusicKey music =
    case music of
        EffortSilent ->
            "none"

        EffortFromPlaylist ->
            "playlist"


effortMusicLabel : EffortMusic -> String
effortMusicLabel music =
    case music of
        EffortSilent ->
            "None"

        EffortFromPlaylist ->
            "Audius playlist"


breakMusicKey : BreakMusic -> String
breakMusicKey music =
    case music of
        BreakSilent ->
            "none"

        BreakDimmedEffort ->
            "dimmed"

        BreakFromPlaylist ->
            "playlist"


breakMusicLabel : BreakMusic -> String
breakMusicLabel music =
    case music of
        BreakSilent ->
            "None"

        BreakDimmedEffort ->
            "Effort music, dimmed"

        BreakFromPlaylist ->
            "Audius playlist"


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
