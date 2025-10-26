module Cmd.Extra exposing (delay, send)

import Process
import Task


delay : Int -> msg -> Cmd msg
delay seconds msg =
    let
        ms =
            seconds * 1000
    in
    Process.sleep (toFloat ms)
        |> Task.perform (\_ -> msg)


send : msg -> Cmd msg
send msg =
    Task.succeed msg
        |> Task.perform identity
