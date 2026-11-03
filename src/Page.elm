module Page exposing (Page(..), view, viewLoading)

import Browser exposing (Document)
import Html exposing (Html, a, div, header, main_, span, text)
import Html.Attributes exposing (class, href)
import Route
import Session exposing (Session)


type Page
    = Other
    | Home
    | RunPlan


appName : String
appName =
    "Workitout"


{-| Wraps a page content into the shared layout: a top bar linking home (hidden on the home page, which has its own title) and the content area.
-}
view : Session -> Page -> { title : String, content : Html msg } -> Document msg
view _ page { title, content } =
    { title = title ++ " :: WorkItOut"
    , body =
        [ div [ class "flex min-h-screen flex-col bg-gray-50 text-gray-800 antialiased" ]
            [ viewTopBar page
            , main_ [ class "flex-1" ] [ content ]
            , viewFooter
            ]
        ]
    }


{-| Generic loading indicator for pages that have no dedicated skeleton.
-}
viewLoading : Html msg
viewLoading =
    div [ class "flex justify-center px-4 py-10" ]
        [ span [ class "animate-pulse text-gray-400" ] [ text "Loading…" ] ]


viewTopBar : Page -> Html msg
viewTopBar page =
    case page of
        Home ->
            text ""

        _ ->
            header [ class "border-b border-gray-200 bg-white" ]
                [ div [ class "mx-auto flex max-w-6xl items-center px-4 py-3" ]
                    [ a
                        [ class "text-xl font-bold tracking-tight text-pink-500 transition-colors hover:text-pink-600"
                        , href (Route.routeToString Route.Home)
                        ]
                        [ text appName ]
                    ]
                ]


viewFooter : Html msg
viewFooter =
    div [] []
