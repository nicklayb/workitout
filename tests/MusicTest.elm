module MusicTest exposing (suite)

{-| Unit tests for the `Music` module: which track plays as steps change and
the user controls the music.
-}

import Audius exposing (Track)
import Expect
import Fixtures
import Music exposing (Channel(..), Command(..), Status(..), StepKind(..))
import Settings exposing (BreakMusic(..), EffortMusic(..), Settings)
import Test exposing (Test, describe, test)


dimmedPercent : Int
dimmedPercent =
    30


dimmedVolume : Float
dimmedVolume =
    0.3


withBreakMusic : BreakMusic -> Settings
withBreakMusic breakMusic =
    Settings.default
        |> Settings.setBreakMusic breakMusic
        |> Settings.setDimmedEffortVolume dimmedPercent


{-| Settings with a playlist for efforts and another one for breaks.
-}
playlistSettings : Settings
playlistSettings =
    withBreakMusic BreakFromPlaylist


{-| Music with both playlists loaded.
-}
loadedMusic : Settings -> Music.Model
loadedMusic settings =
    Music.init settings
        |> Music.tracksFetched EffortChannel (Just Fixtures.effortTracks)
        |> Tuple.first
        |> Music.tracksFetched BreakChannel (Just Fixtures.breakTracks)
        |> Tuple.first


{-| Runs the steps one after the other, collecting the commands of each.
-}
runSteps : Settings -> List StepKind -> Music.Model -> ( Music.Model, List (List Command) )
runSteps settings stepKinds music =
    List.foldl
        (\stepKind ( currentMusic, commands ) ->
            Music.enterStep settings stepKind currentMusic
                |> Tuple.mapSecond (\stepCommands -> commands ++ [ stepCommands ])
        )
        ( music, [] )
        stepKinds


playedUrl : List Command -> Maybe String
playedUrl commands =
    case commands of
        [ Play { url } ] ->
            Just url

        _ ->
            Nothing


playedChannelAndVolume : List Command -> Maybe ( Channel, Float )
playedChannelAndVolume commands =
    case commands of
        [ Play { channel, volume } ] ->
            Just ( channel, volume )

        _ ->
            Nothing


isTrackOf : List Track -> Maybe String -> Bool
isTrackOf tracks maybeUrl =
    case maybeUrl of
        Just url ->
            List.any (\track -> Audius.streamUrl track == url) tracks

        Nothing ->
            False


{-| Music playing an effort track, with the URL of that track.
-}
playingEffort : Settings -> ( Music.Model, String )
playingEffort settings =
    let
        ( music, commands ) =
            Music.enterStep settings EffortStep (loadedMusic settings)
    in
    ( music, playedUrl commands |> Maybe.withDefault "" )


suite : Test
suite =
    describe "Music"
        [ describe "playlistsToFetch"
            [ test "fetches both playlists when breaks have their own" <|
                \_ ->
                    Music.playlistsToFetch playlistSettings
                        |> List.map Tuple.first
                        |> Expect.equal [ EffortChannel, BreakChannel ]
            , test "only fetches the effort playlist by default" <|
                \_ ->
                    Music.playlistsToFetch Settings.default
                        |> List.map Tuple.first
                        |> Expect.equal [ EffortChannel ]
            , test "only fetches the effort playlist when breaks dim it" <|
                \_ ->
                    Music.playlistsToFetch (withBreakMusic BreakDimmedEffort)
                        |> List.map Tuple.first
                        |> Expect.equal [ EffortChannel ]
            , test "fetches nothing when everything is silent" <|
                \_ ->
                    Music.playlistsToFetch (withBreakMusic BreakSilent |> Settings.setEffortMusic EffortSilent)
                        |> Expect.equal []
            ]
        , describe "enterStep"
            [ test "plays an effort track at full volume" <|
                \_ ->
                    Music.enterStep playlistSettings EffortStep (loadedMusic playlistSettings)
                        |> Tuple.second
                        |> Expect.all
                            [ playedUrl >> isTrackOf Fixtures.effortTracks >> Expect.equal True
                            , playedChannelAndVolume >> Expect.equal (Just ( EffortChannel, 1 ))
                            ]
            , test "keeps the same track across consecutive efforts" <|
                \_ ->
                    let
                        ( _, commands ) =
                            runSteps playlistSettings [ EffortStep, EffortStep ] (loadedMusic playlistSettings)
                    in
                    case List.map playedUrl commands of
                        [ first, second ] ->
                            Expect.all [ \_ -> second |> Expect.equal first, \_ -> first |> Expect.notEqual Nothing ] ()

                        _ ->
                            Expect.fail "Expected two steps"
            , test "plays a break track during a break" <|
                \_ ->
                    runSteps playlistSettings [ EffortStep, BreakStep ] (loadedMusic playlistSettings)
                        |> Tuple.second
                        |> List.drop 1
                        |> List.concat
                        |> Expect.all
                            [ playedUrl >> isTrackOf Fixtures.breakTracks >> Expect.equal True
                            , playedChannelAndVolume >> Expect.equal (Just ( BreakChannel, 1 ))
                            ]
            , test "goes back to the effort track after a break" <|
                \_ ->
                    case runSteps playlistSettings [ EffortStep, BreakStep, EffortStep ] (loadedMusic playlistSettings) |> Tuple.second of
                        [ first, _, third ] ->
                            playedUrl third |> Expect.equal (playedUrl first)

                        _ ->
                            Expect.fail "Expected three steps"
            , test "dims the effort track during a break" <|
                \_ ->
                    let
                        settings =
                            withBreakMusic BreakDimmedEffort
                    in
                    case runSteps settings [ EffortStep, BreakStep ] (loadedMusic settings) |> Tuple.second of
                        [ effort, break ] ->
                            Expect.all
                                [ \_ -> playedUrl break |> Expect.equal (playedUrl effort)
                                , \_ -> playedChannelAndVolume break |> Expect.equal (Just ( EffortChannel, dimmedVolume ))
                                ]
                                ()

                        _ ->
                            Expect.fail "Expected two steps"
            , test "pauses during a silent break" <|
                \_ ->
                    let
                        settings =
                            withBreakMusic BreakSilent

                        ( music, commands ) =
                            runSteps settings [ EffortStep, BreakStep ] (loadedMusic settings)
                    in
                    ( List.drop 1 commands, Music.status music ) |> Expect.equal ( [ [ Pause ] ], Silent )
            , test "dimmed breaks stay silent without effort music" <|
                \_ ->
                    let
                        settings =
                            withBreakMusic BreakDimmedEffort |> Settings.setEffortMusic EffortSilent
                    in
                    Music.enterStep settings BreakStep (loadedMusic settings)
                        |> Tuple.second
                        |> Expect.equal [ Pause ]
            , test "waits for the tracks while they load" <|
                \_ ->
                    Music.enterStep playlistSettings EffortStep (Music.init playlistSettings)
                        |> Tuple.mapFirst Music.status
                        |> Expect.equal ( LoadingTracks, [ Pause ] )
            ]
        , describe "tracksFetched"
            [ test "starts the tracks the current step waits for" <|
                \_ ->
                    Music.enterStep playlistSettings EffortStep (Music.init playlistSettings)
                        |> Tuple.first
                        |> Music.tracksFetched EffortChannel (Just Fixtures.effortTracks)
                        |> Tuple.second
                        |> playedUrl
                        |> isTrackOf Fixtures.effortTracks
                        |> Expect.equal True
            , test "doesn't play the tracks of another channel" <|
                \_ ->
                    Music.enterStep playlistSettings EffortStep (Music.init playlistSettings)
                        |> Tuple.first
                        |> Music.tracksFetched BreakChannel (Just Fixtures.breakTracks)
                        |> Tuple.second
                        |> Expect.equal []
            , test "reports a playlist that failed to load" <|
                \_ ->
                    Music.enterStep playlistSettings EffortStep (Music.init playlistSettings)
                        |> Tuple.first
                        |> Music.tracksFetched EffortChannel Nothing
                        |> Tuple.first
                        |> Music.status
                        |> Expect.equal Unavailable
            ]
        , describe "controls"
            [ test "pausing pauses the player" <|
                \_ ->
                    playingEffort playlistSettings
                        |> Tuple.first
                        |> Music.togglePause
                        |> Tuple.second
                        |> Expect.equal [ Pause ]
            , test "a pause lasts across steps" <|
                \_ ->
                    playingEffort playlistSettings
                        |> Tuple.first
                        |> Music.togglePause
                        |> Tuple.first
                        |> Music.enterStep playlistSettings BreakStep
                        |> Tuple.second
                        |> Expect.equal [ Pause ]
            , test "resuming plays the same track again" <|
                \_ ->
                    let
                        ( music, url ) =
                            playingEffort playlistSettings
                    in
                    music
                        |> Music.togglePause
                        |> Tuple.first
                        |> Music.togglePause
                        |> Tuple.second
                        |> playedUrl
                        |> Expect.equal (Just url)
            , test "next plays another track of the same playlist" <|
                \_ ->
                    let
                        ( music, url ) =
                            playingEffort playlistSettings

                        nextUrl =
                            Music.next music |> Tuple.second |> playedUrl
                    in
                    Expect.all
                        [ \_ -> nextUrl |> Expect.notEqual (Just url)
                        , \_ -> isTrackOf Fixtures.effortTracks nextUrl |> Expect.equal True
                        ]
                        ()
            , test "stop silences the music" <|
                \_ ->
                    playingEffort playlistSettings
                        |> Tuple.first
                        |> Music.stop
                        |> Tuple.mapFirst Music.status
                        |> Expect.equal ( Silent, [ Pause ] )
            ]
        , describe "player events"
            [ test "a finished track is followed by another one" <|
                \_ ->
                    let
                        ( music, url ) =
                            playingEffort playlistSettings
                    in
                    Music.trackEnded url music
                        |> Tuple.second
                        |> playedUrl
                        |> Expect.all [ Expect.notEqual (Just url), isTrackOf Fixtures.effortTracks >> Expect.equal True ]
            , test "the end of a track no longer playing is ignored" <|
                \_ ->
                    playingEffort playlistSettings
                        |> Tuple.first
                        |> Music.trackEnded "https://audius.co/moe/flaming-moe"
                        |> Tuple.second
                        |> Expect.equal []
            , test "a failing track is dropped" <|
                \_ ->
                    let
                        singleTrackMusic =
                            Music.init playlistSettings
                                |> Music.tracksFetched EffortChannel (Just (List.take 1 Fixtures.effortTracks))
                                |> Tuple.first

                        ( music, commands ) =
                            Music.enterStep playlistSettings EffortStep singleTrackMusic
                    in
                    Music.trackFailed (playedUrl commands |> Maybe.withDefault "") music
                        |> Tuple.mapFirst Music.status
                        |> Expect.equal ( Unavailable, [ Pause ] )
            ]
        ]
