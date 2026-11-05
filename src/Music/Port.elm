port module Music.Port exposing (run, stop, trackEnded, trackFailed)

{-| Side effects driving the background audio player of `src/index.js`.
-}

import Json.Encode as Encode
import Music exposing (Channel(..), Command(..))


port controlMusic : Encode.Value -> Cmd msg


port musicTrackEnded : (String -> msg) -> Sub msg


port musicTrackFailed : (String -> msg) -> Sub msg


{-| Sends the commands to the audio player.
-}
run : List Command -> Cmd msg
run commands =
    Cmd.batch (List.map (encode >> controlMusic) commands)


{-| Pauses the audio player.
-}
stop : Cmd msg
stop =
    run [ Pause ]


{-| Notifies, with its URL, that a track reached its end.
-}
trackEnded : (String -> msg) -> Sub msg
trackEnded =
    musicTrackEnded


{-| Notifies, with its URL, that a track can't be played.
-}
trackFailed : (String -> msg) -> Sub msg
trackFailed =
    musicTrackFailed


encode : Command -> Encode.Value
encode command =
    case command of
        Play { channel, url, volume } ->
            Encode.object
                [ ( "action", Encode.string "play" )
                , ( "channel", Encode.string (channelKey channel) )
                , ( "url", Encode.string url )
                , ( "volume", Encode.float volume )
                ]

        Pause ->
            Encode.object [ ( "action", Encode.string "pause" ) ]


channelKey : Channel -> String
channelKey channel =
    case channel of
        EffortChannel ->
            "effort"

        BreakChannel ->
            "break"
