module Session exposing (Session, appConfig, init, navKey, putSession, putUrl, url, urlChanged)

import AppConfig exposing (AppConfig)
import Browser.Navigation as Nav
import Route exposing (Route(..))
import Url exposing (Url)


type alias Session =
    { navKey : Nav.Key
    , url : Url
    , appConfig : AppConfig
    }


type alias WithSession s =
    { s | session : Session }


putSession : Session -> WithSession s -> WithSession s
putSession session withSession =
    { withSession | session = session }


init : Nav.Key -> Url -> AppConfig -> Session
init navKeyInput urlInput appConfigInput =
    { navKey = navKeyInput
    , url = urlInput
    , appConfig = appConfigInput
    }


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
