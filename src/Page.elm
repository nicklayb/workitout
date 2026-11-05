module Page exposing (Page(..), view, viewLoading)

import Browser exposing (Document)
import Html exposing (Html, a, div, footer, header, main_, span, text)
import Html.Attributes exposing (attribute, class, href, rel, target)
import Route
import Session exposing (Session)
import Ui.Icon as Icon


type Page
    = Other
    | Home
    | RunPlan
    | Settings
    | PlanEditor


appName : String
appName =
    "Workitout"


repositoryUrl : String
repositoryUrl =
    "https://github.com/nicklayb/workitout"


{-| Wraps a page content into the shared layout: a top bar linking home (the
name is hidden on the home page, which has its own title) and settings, the
given application-wide `notices`, the content area and a footer.
-}
view : Session -> Page -> List (Html msg) -> { title : String, content : Html msg } -> Document msg
view session page notices { title, content } =
    { title = title ++ " :: WorkItOut"
    , body =
        [ div [ class "flex min-h-screen flex-col bg-gray-50 text-gray-800 antialiased" ]
            [ viewTopBar page
            , viewNotices notices
            , main_ [ class "flex-1" ] [ content ]
            , viewFooter (Session.appConfig session).version
            ]
        ]
    }


{-| Generic loading indicator for pages that have no dedicated skeleton.
-}
viewLoading : Html msg
viewLoading =
    div [ class "flex justify-center px-4 py-10" ]
        [ span [ class "animate-pulse text-gray-400" ] [ text "Loading…" ] ]


viewNotices : List (Html msg) -> Html msg
viewNotices notices =
    if List.isEmpty notices then
        text ""

    else
        div [ class "mx-auto flex w-full max-w-6xl flex-col gap-2 px-4 pt-4" ] notices


viewTopBar : Page -> Html msg
viewTopBar page =
    header [ class (topBarClasses page) ]
        [ div [ class "mx-auto flex max-w-6xl items-center justify-between px-4 py-3" ]
            [ viewAppName page
            , viewSettingsLink page
            ]
        ]


topBarClasses : Page -> String
topBarClasses page =
    case page of
        Home ->
            ""

        _ ->
            "border-b border-gray-200 bg-white"


viewAppName : Page -> Html msg
viewAppName page =
    case page of
        Home ->
            span [] []

        _ ->
            a
                [ class "text-xl font-bold tracking-tight text-pink-500 transition-colors hover:text-pink-600"
                , href (Route.routeToString Route.Home)
                ]
                [ text appName ]


viewSettingsLink : Page -> Html msg
viewSettingsLink page =
    let
        stateClasses =
            case page of
                Settings ->
                    "bg-pink-50 text-pink-600"

                _ ->
                    "text-gray-400 hover:bg-pink-50 hover:text-pink-600"
    in
    a
        [ class ("flex size-10 items-center justify-center rounded-full transition-colors " ++ stateClasses)
        , href (Route.routeToString Route.Settings)
        , attribute "aria-label" "Settings"
        ]
        [ Icon.gear "size-6" ]


{-| Discreet footer with a link to the repository, the origin of the app and
its version.
-}
viewFooter : String -> Html msg
viewFooter version =
    footer [ class "px-4 py-6" ]
        [ div [ class "mx-auto flex max-w-6xl flex-wrap items-center justify-center gap-x-2 gap-y-1 text-xs text-gray-400" ]
            [ a
                [ href repositoryUrl
                , target "_blank"
                , rel "noopener noreferrer"
                , class "transition-colors hover:text-pink-500"
                ]
                [ text "GitHub" ]
            , viewFooterSeparator
            , span [] [ text "Made with ❤️ from Shawinigan" ]
            , viewFooterSeparator
            , span [ class "tabular-nums" ] [ text ("v" ++ version) ]
            ]
        ]


viewFooterSeparator : Html msg
viewFooterSeparator =
    span [ attribute "aria-hidden" "true" ] [ text "·" ]
