module Session exposing (Session, appConfig, currentDay, init, navKey, putSession, putUrl, url, urlChanged)

import AppConfig exposing (AppConfig)
import Browser.Navigation as Nav
import Resource exposing (Resource(..))
import Route exposing (Route(..))
import Time
import Url exposing (Url)


type alias Session =
    { navKey : Nav.Key
    , url : Url
    , appConfig : AppConfig
    , currentDay : Time.Weekday
    }


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
