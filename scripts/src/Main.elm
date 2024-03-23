module Main exposing (..)

import BackendTask exposing (BackendTask)
import BackendTask.File as File
import BackendTask.Glob as Glob
import Elm
import FatalError exposing (FatalError)
import Hex.Convert
import Pages.Script as Script
import String.Extra


run : Script.Script
run =
    Script.withoutCliOptions task


task : BackendTask FatalError ()
task =
    Glob.succeed (\filename fullPath -> { filename = filename, fullPath = fullPath })
        |> Glob.match (Glob.literal "tests/Sources/")
        |> Glob.capture Glob.wildcard
        |> Glob.match (Glob.literal ".mp3")
        |> Glob.captureFilePath
        |> Glob.toBackendTask
        |> BackendTask.andThen
            (\mp3s ->
                Script.log ("Found " ++ String.fromInt (List.length mp3s) ++ " files")
                    |> BackendTask.andThen
                        (\_ ->
                            mp3s
                                |> List.map
                                    (\{ fullPath, filename } ->
                                        File.binaryFile fullPath
                                            |> BackendTask.allowFatal
                                            |> BackendTask.andThen
                                                (\bytes ->
                                                    let
                                                        file =
                                                            bytes
                                                                |> Hex.Convert.toString
                                                                |> Elm.string
                                                                |> Elm.declaration "raw"
                                                                |> Elm.expose
                                                                |> List.singleton
                                                                |> Elm.file
                                                                    [ "Sources"
                                                                    , filename
                                                                        |> String.replace " " ""
                                                                        |> String.replace "-" "_"
                                                                        |> String.replace "." "_"
                                                                        |> String.Extra.toSentenceCase
                                                                    ]
                                                    in
                                                    Script.log (filename ++ ".mp3")
                                                        |> BackendTask.andThen
                                                            (\_ ->
                                                                Script.writeFile
                                                                    { path = "tests/" ++ file.path
                                                                    , body = file.contents
                                                                    }
                                                                    |> BackendTask.allowFatal
                                                            )
                                                )
                                    )
                                |> BackendTask.combine
                        )
            )
        |> BackendTask.map (\_ -> ())
