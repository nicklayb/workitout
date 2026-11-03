module Page.Errors.NotFound exposing (view)

import Html exposing (Html, a, div, h1, p, text)
import Html.Attributes exposing (class, href)
import Route


view : { title : String, content : Html msg }
view =
    { title = "Page Not Found"
    , content = viewContent
    }


viewContent : Html msg
viewContent =
    div [ class "bg-gray-50 px-4 py-10" ]
        [ div [ class "mx-auto max-w-md rounded-2xl bg-white px-6 py-10 text-center shadow-sm ring-1 ring-gray-200" ]
            [ p [ class "text-5xl font-bold tracking-tight text-pink-500" ] [ text "404" ]
            , h1 [ class "mt-3 text-2xl font-bold tracking-tight text-gray-800" ] [ text "Page not found" ]
            , p [ class "mt-2 mb-6 text-gray-500" ] [ text "The page you are looking for does not exist." ]
            , a
                [ class "inline-flex items-center justify-center rounded-full bg-pink-500 px-6 py-2.5 font-semibold text-white shadow-sm transition-colors hover:bg-pink-600"
                , href (Route.routeToString Route.Home)
                ]
                [ text "Back home" ]
            ]
        ]
