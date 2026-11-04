module Session exposing (Notice(..), Session, appConfig, currentDay, init, navKey, notice, putNotice, putSession, putSettings, putUrl, settings, url, urlChanged)

import AppConfig exposing (AppConfig)
import Browser.Navigation as Nav
import Resource exposing (Resource(..))
import Route exposing (Route(..))
import Settings exposing (Settings)
import Time
import Url exposing (Url)


type alias Session =
    { navKey : Nav.Key
    , url : Url
    , appConfig : AppConfig
    , currentDay : Time.Weekday
    , settings : Settings
    , notice : Maybe Notice
    }


{-| Application-wide message shown above every page until dismissed.
-}
type Notice
    = SavedSettingsInvalid
    | SettingsMigrated


type alias WithSession s =
    { s | session : Session }


putSession : Session -> WithSession s -> WithSession s
putSession session withSession =
    { withSession | session = session }


init : Nav.Key -> Url -> AppConfig -> Session
init navKeyInput urlInput appConfigInput =
    let
        weekday =
            case appConfigInput.weekday of
                0 ->
                    Time.Sun

                1 ->
                    Time.Mon

                2 ->
                    Time.Tue

                3 ->
                    Time.Wed

                4 ->
                    Time.Thu

                5 ->
                    Time.Fri

                _ ->
                    Time.Sat
    in
    { navKey = navKeyInput
    , url = urlInput
    , appConfig = appConfigInput
    , currentDay = weekday
    , settings = Settings.default
    , notice = Nothing
    }


currentDay : Session -> Time.Weekday
currentDay session =
    session.currentDay


appConfig : Session -> AppConfig
appConfig session =
    session.appConfig


navKey : Session -> Nav.Key
navKey session =
    session.navKey


url : Session -> Url
url session =
    session.url


urlChanged : Url -> Session -> Bool
urlChanged compareUrl session =
    Route.urlChanged compareUrl session.url


putUrl : Url -> Session -> Session
putUrl urlInput session =
    { session | url = urlInput }


{-| Settings currently in use.
-}
settings : Session -> Settings
settings session =
    session.settings


putSettings : Settings -> Session -> Session
putSettings settingsInput session =
    { session | settings = settingsInput }


{-| Application-wide notice to display, if any.
-}
notice : Session -> Maybe Notice
notice session =
    session.notice


putNotice : Maybe Notice -> Session -> Session
putNotice noticeInput session =
    { session | notice = noticeInput }
