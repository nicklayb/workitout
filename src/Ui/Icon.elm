module Ui.Icon exposing (arrowPath, chevronDown, chevronUp, clock, fork, forward, gear, musicalNote, pause, pencil, play, speakerWave, sun)

{-| Inline SVG outline icons, drawn with `currentColor` so they follow the text
color and sized through the given classes. The gear, chevron, pencil and media paths come
from Heroicons (MIT license).
-}

import Html exposing (Html)
import Svg exposing (path, svg)
import Svg.Attributes exposing (class, d, fill, stroke, strokeLinecap, strokeLinejoin, strokeWidth, viewBox)


gearOutlinePath : String
gearOutlinePath =
    "M9.594 3.94c.09-.542.56-.94 1.11-.94h2.593c.55 0 1.02.398 1.11.94l.213 1.281c.063.374.313.686.645.87.074.04.147.083.22.127.325.196.72.257 1.075.124l1.217-.456a1.125 1.125 0 0 1 1.37.49l1.296 2.247a1.125 1.125 0 0 1-.26 1.431l-1.003.827c-.293.241-.438.613-.43.992a7.723 7.723 0 0 1 0 .255c-.008.378.137.75.43.991l1.004.827c.424.35.534.955.26 1.43l-1.298 2.247a1.125 1.125 0 0 1-1.369.491l-1.217-.456c-.355-.133-.75-.072-1.076.124a6.47 6.47 0 0 1-.22.128c-.331.183-.581.495-.644.869l-.213 1.281c-.09.543-.56.94-1.11.94h-2.594c-.55 0-1.019-.398-1.11-.94l-.213-1.281c-.062-.374-.312-.686-.644-.87a6.52 6.52 0 0 1-.22-.127c-.325-.196-.72-.257-1.076-.124l-1.217.456a1.125 1.125 0 0 1-1.369-.49l-1.297-2.247a1.125 1.125 0 0 1 .26-1.431l1.004-.827c.292-.24.437-.613.43-.991a6.932 6.932 0 0 1 0-.255c.007-.38-.138-.751-.43-.992l-1.004-.827a1.125 1.125 0 0 1-.26-1.43l1.297-2.247a1.125 1.125 0 0 1 1.37-.491l1.216.456c.356.133.751.072 1.076-.124.072-.044.146-.086.22-.128.332-.183.582-.495.644-.869l.214-1.28Z"


gearCenterPath : String
gearCenterPath =
    "M15 12a3 3 0 1 1-6 0 3 3 0 0 1 6 0Z"


{-| Two branches leaving a trunk: circles on the three ends, joined by lines.
-}
forkPaths : List String
forkPaths =
    [ "M8 5a2 2 0 1 1-4 0 2 2 0 0 1 4 0Z"
    , "M20 5a2 2 0 1 1-4 0 2 2 0 0 1 4 0Z"
    , "M14 19a2 2 0 1 1-4 0 2 2 0 0 1 4 0Z"
    , "M6 7v1a3 3 0 0 0 3 3h6a3 3 0 0 0 3-3V7"
    , "M12 11v6"
    ]


outline : List String -> String -> Html msg
outline paths classes =
    svg
        [ viewBox "0 0 24 24"
        , fill "none"
        , stroke "currentColor"
        , strokeWidth "1.5"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        , class classes
        ]
        (List.map (\pathDefinition -> path [ d pathDefinition ] []) paths)


gear : String -> Html msg
gear =
    outline [ gearOutlinePath, gearCenterPath ]


{-| Fork icon, used to copy a community plan into the editor.
-}
fork : String -> Html msg
fork =
    outline forkPaths


{-| Pencil icon, used to edit a plan.
-}
pencil : String -> Html msg
pencil =
    outline [ "m16.862 4.487 1.687-1.688a1.875 1.875 0 1 1 2.652 2.652L6.832 19.82a4.5 4.5 0 0 1-1.897 1.13l-2.685.8.8-2.685a4.5 4.5 0 0 1 1.13-1.897L16.863 4.487Zm0 0L19.5 7.125" ]


chevronUp : String -> Html msg
chevronUp =
    outline [ "m4.5 15.75 7.5-7.5 7.5 7.5" ]


chevronDown : String -> Html msg
chevronDown =
    outline [ "m19.5 8.25-7.5 7.5-7.5-7.5" ]


{-| Play icon, used to resume the music.
-}
play : String -> Html msg
play =
    outline [ "M5.25 5.653c0-.856.917-1.398 1.667-.986l11.54 6.347a1.125 1.125 0 0 1 0 1.972l-11.54 6.347a1.125 1.125 0 0 1-1.667-.986V5.653Z" ]


{-| Pause icon, used to pause the music.
-}
pause : String -> Html msg
pause =
    outline [ "M15.75 5.25v13.5m-7.5-13.5v13.5" ]


{-| Forward icon, used to skip to the next track.
-}
forward : String -> Html msg
forward =
    outline [ "M3 8.689c0-.864.933-1.406 1.683-.977l7.108 4.061a1.125 1.125 0 0 1 0 1.954l-7.108 4.061A1.125 1.125 0 0 1 3 16.811V8.69ZM12.75 8.689c0-.864.933-1.406 1.683-.977l7.108 4.061a1.125 1.125 0 0 1 0 1.954l-7.108 4.061a1.125 1.125 0 0 1-1.683-.977V8.69Z" ]


{-| Musical note icon, shown next to the playing track.
-}
musicalNote : String -> Html msg
musicalNote =
    outline [ "m9 9 10.5-3m0 6.553v3.75a2.25 2.25 0 0 1-1.632 2.163l-1.32.377a1.803 1.803 0 1 1-.99-3.467l2.31-.66a2.25 2.25 0 0 0 1.632-2.163Zm0 0V2.25L9 5.25v10.303m0 0v3.75a2.25 2.25 0 0 1-1.632 2.163l-1.32.377a1.803 1.803 0 0 1-.99-3.467l2.31-.66A2.25 2.25 0 0 0 9 15.553Z" ]


{-| Clock icon, used for the pre-round delay.
-}
clock : String -> Html msg
clock =
    outline [ "M12 6v6h4.5m4.5 0a9 9 0 1 1-18 0 9 9 0 0 1 18 0Z" ]


{-| Circular arrows icon, used for moving to the next round automatically.
-}
arrowPath : String -> Html msg
arrowPath =
    outline [ "M16.023 9.348h4.992v-.001M2.985 19.644v-4.992m0 0h4.992m-4.993 0 3.181 3.183a8.25 8.25 0 0 0 13.803-3.7M4.031 9.865a8.25 8.25 0 0 1 13.803-3.7l3.181 3.182m0-4.991v4.99" ]


{-| Speaker icon, used for sound effects.
-}
speakerWave : String -> Html msg
speakerWave =
    outline [ "M19.114 5.636a9 9 0 0 1 0 12.728M16.463 8.288a5.25 5.25 0 0 1 0 7.424M6.75 8.25l4.72-4.72a.75.75 0 0 1 1.28.53v15.88a.75.75 0 0 1-1.28.53l-4.72-4.72H4.51c-.88 0-1.704-.507-1.938-1.354A9.009 9.009 0 0 1 2.25 12c0-.83.112-1.633.322-2.396C2.806 8.756 3.63 8.25 4.51 8.25H6.75Z" ]


{-| Sun icon, used for keeping the screen awake.
-}
sun : String -> Html msg
sun =
    outline [ "M12 3v2.25m6.364.386-1.591 1.591M21 12h-2.25m-.386 6.364-1.591-1.591M12 18.75V21m-4.773-4.227-1.591 1.591M5.25 12H3m4.227-4.773L5.636 5.636M15.75 12a3.75 3.75 0 1 1-7.5 0 3.75 3.75 0 0 1 7.5 0Z" ]
