module Music exposing
    ( Channel(..)
    , Command(..)
    , Model
    , Status(..)
    , StepKind(..)
    , enterStep
    , init
    , next
    , playlistsToFetch
    , seed
    , status
    , stop
    , togglePause
    , trackEnded
    , trackFailed
    , tracksFetched
    )

{-| Decides which music plays during a workout. Every function is pure: they
return the `Command`s to send to the audio player, which `Music.Port` runs.

Each `Channel` remembers its current track, so going back to a channel
resumes its track instead of picking a new one.

-}

import Audius exposing (Track)
import Audius.Playlist exposing (Playlist)
import Random
import Settings exposing (BreakMusic(..), EffortMusic(..), Settings)


{-| Source of music: the effort playlist or the break playlist.
-}
type Channel
    = EffortChannel
    | BreakChannel


{-| Kind of workout step that starts.
-}
type StepKind
    = EffortStep
    | BreakStep


{-| Instruction for the audio player. `Play` resumes the track of the channel
where it was left when it is the same track, and starts it otherwise.
-}
type Command
    = Play { channel : Channel, url : String, volume : Float }
    | Pause


{-| What the music bar shows.
-}
type Status
    = Silent
    | LoadingTracks
    | Unavailable
    | Playing Track
    | Paused Track


type Tracks
    = Unused
    | Loading
    | Loaded (List Track)
    | Failed


type alias ChannelState =
    { tracks : Tracks
    , current : Maybe Track
    }


type alias Output =
    { channel : Channel
    , volume : Float
    }


type Model
    = Model
        { effort : ChannelState
        , break : ChannelState
        , output : Maybe Output
        , isPaused : Bool
        , seed : Random.Seed
        }


fullVolume : Float
fullVolume =
    1


percentDivisor : Float
percentDivisor =
    100


{-| Seed used until `seed` provides a random one.
-}
initialSeed : Int
initialSeed =
    0


{-| Silent music, waiting for the tracks of the playlists the settings use.
-}
init : Settings -> Model
init settings =
    let
        initialChannel channel =
            { tracks =
                if List.any (Tuple.first >> (==) channel) (playlistsToFetch settings) then
                    Loading

                else
                    Unused
            , current = Nothing
            }
    in
    Model
        { effort = initialChannel EffortChannel
        , break = initialChannel BreakChannel
        , output = Nothing
        , isPaused = False
        , seed = Random.initialSeed initialSeed
        }


{-| Playlists whose tracks must be fetched, with the channel they feed.
-}
playlistsToFetch : Settings -> List ( Channel, Playlist )
playlistsToFetch settings =
    List.filterMap identity
        [ if Settings.effortMusic settings == EffortFromPlaylist then
            Just ( EffortChannel, Settings.effortPlaylist settings )

          else
            Nothing
        , if Settings.breakMusic settings == BreakFromPlaylist then
            Just ( BreakChannel, Settings.breakPlaylist settings )

          else
            Nothing
        ]


{-| Replaces the seed used to pick tracks.
-}
seed : Random.Seed -> Model -> Model
seed newSeed (Model model) =
    Model { model | seed = newSeed }


{-| Stores the fetched tracks of a channel, starting them if the channel is
the one expected to play.
-}
tracksFetched : Channel -> Maybe (List Track) -> Model -> ( Model, List Command )
tracksFetched channel maybeTracks model =
    let
        tracks =
            Maybe.map Loaded maybeTracks |> Maybe.withDefault Failed

        updated =
            updateChannel channel (\state -> { state | tracks = tracks }) model
    in
    if isOutputChannel channel updated && currentTrack channel updated == Nothing then
        resume updated

    else
        ( updated, [] )


{-| Switches to the music of the starting step.
-}
enterStep : Settings -> StepKind -> Model -> ( Model, List Command )
enterStep settings stepKind (Model model) =
    resume (Model { model | output = outputFor settings stepKind })


{-| Silences the music until the next step starts.
-}
stop : Model -> ( Model, List Command )
stop (Model model) =
    ( Model { model | output = Nothing }, [ Pause ] )


{-| Pauses the music, or resumes it. A pause lasts across steps until the user
resumes it.
-}
togglePause : Model -> ( Model, List Command )
togglePause (Model model) =
    if model.isPaused then
        resume (Model { model | isPaused = False })

    else
        ( Model { model | isPaused = True }, [ Pause ] )


{-| Skips to another random track of the playing channel, resuming the music
if it was paused.
-}
next : Model -> ( Model, List Command )
next (Model model) =
    case model.output of
        Just output ->
            resume (Model { model | isPaused = False } |> pickTrack output.channel)

        Nothing ->
            ( Model model, [] )


{-| Moves to another track when the track at `url` reaches its end.
-}
trackEnded : String -> Model -> ( Model, List Command )
trackEnded url model =
    case channelPlaying url model of
        Just channel ->
            resume (pickTrack channel model)

        Nothing ->
            ( model, [] )


{-| Drops the track at `url`, which can't be played, and moves to another one
if it was playing.
-}
trackFailed : String -> Model -> ( Model, List Command )
trackFailed url model =
    let
        withoutTrack =
            model
                |> removeTrack url EffortChannel
                |> removeTrack url BreakChannel
    in
    case channelPlaying url model of
        Just channel ->
            resume (pickTrack channel withoutTrack)

        Nothing ->
            ( withoutTrack, [] )


{-| What the music is doing.
-}
status : Model -> Status
status (Model model) =
    case model.output of
        Nothing ->
            Silent

        Just output ->
            let
                state =
                    channelState output.channel (Model model)
            in
            case ( state.current, state.tracks ) of
                ( Just track, _ ) ->
                    if model.isPaused then
                        Paused track

                    else
                        Playing track

                ( Nothing, Loading ) ->
                    LoadingTracks

                ( Nothing, _ ) ->
                    Unavailable


{-| Channel and volume of the music of a step, if any.
-}
outputFor : Settings -> StepKind -> Maybe Output
outputFor settings stepKind =
    let
        effortOutput volume =
            if Settings.effortMusic settings == EffortFromPlaylist then
                Just { channel = EffortChannel, volume = volume }

            else
                Nothing
    in
    case ( stepKind, Settings.breakMusic settings ) of
        ( EffortStep, _ ) ->
            effortOutput fullVolume

        ( BreakStep, BreakSilent ) ->
            Nothing

        ( BreakStep, BreakDimmedEffort ) ->
            effortOutput (toFloat (Settings.dimmedEffortVolumePercent settings) / percentDivisor)

        ( BreakStep, BreakFromPlaylist ) ->
            Just { channel = BreakChannel, volume = fullVolume }


{-| Plays the current track of the output channel, picking one when it has
none. Pauses when the music is paused or there is nothing to play.
-}
resume : Model -> ( Model, List Command )
resume (Model model) =
    case ( model.output, model.isPaused ) of
        ( Just output, False ) ->
            let
                withTrack =
                    if currentTrack output.channel (Model model) == Nothing then
                        pickTrack output.channel (Model model)

                    else
                        Model model
            in
            case currentTrack output.channel withTrack of
                Just track ->
                    ( withTrack, [ Play { channel = output.channel, url = Audius.streamUrl track, volume = output.volume } ] )

                Nothing ->
                    ( withTrack, [ Pause ] )

        _ ->
            ( Model model, [ Pause ] )


{-| Makes a random track the current one of a channel, avoiding the current
track when there is another one.
-}
pickTrack : Channel -> Model -> Model
pickTrack channel (Model model) =
    let
        state =
            channelState channel (Model model)

        tracks =
            case state.tracks of
                Loaded loadedTracks ->
                    loadedTracks

                _ ->
                    []

        candidates =
            case List.filter (\track -> Just track /= state.current) tracks of
                [] ->
                    tracks

                otherTracks ->
                    otherTracks

        ( index, newSeed ) =
            Random.step (Random.int 0 (List.length candidates - 1)) model.seed

        picked =
            List.drop index candidates |> List.head
    in
    Model { model | seed = newSeed }
        |> updateChannel channel (\channelValue -> { channelValue | current = picked })


removeTrack : String -> Channel -> Model -> Model
removeTrack url channel model =
    let
        isOtherTrack track =
            Audius.streamUrl track /= url

        remove state =
            { tracks =
                case state.tracks of
                    Loaded tracks ->
                        Loaded (List.filter isOtherTrack tracks)

                    other ->
                        other
            , current =
                state.current
                    |> Maybe.andThen
                        (\track ->
                            if isOtherTrack track then
                                Just track

                            else
                                Nothing
                        )
            }
    in
    updateChannel channel remove model


{-| Channel currently playing the track at `url`, if any.
-}
channelPlaying : String -> Model -> Maybe Channel
channelPlaying url (Model model) =
    model.output
        |> Maybe.map .channel
        |> Maybe.andThen
            (\channel ->
                if Maybe.map Audius.streamUrl (currentTrack channel (Model model)) == Just url then
                    Just channel

                else
                    Nothing
            )


isOutputChannel : Channel -> Model -> Bool
isOutputChannel channel (Model model) =
    Maybe.map .channel model.output == Just channel


currentTrack : Channel -> Model -> Maybe Track
currentTrack channel model =
    (channelState channel model).current


channelState : Channel -> Model -> ChannelState
channelState channel (Model model) =
    case channel of
        EffortChannel ->
            model.effort

        BreakChannel ->
            model.break


updateChannel : Channel -> (ChannelState -> ChannelState) -> Model -> Model
updateChannel channel transform (Model model) =
    case channel of
        EffortChannel ->
            Model { model | effort = transform model.effort }

        BreakChannel ->
            Model { model | break = transform model.break }
