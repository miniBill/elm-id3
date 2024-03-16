module Tests exposing (suite)

import Bytes exposing (Bytes)
import Bytes.Encode
import Expect
import Hex.Convert
import ID3 exposing (Frame(..))
import Sources.Bauchamp128
import Sources.LenaRaine_Celeste_Prologue
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Main test suite"
        [ tagsTest "Bauchamp128.mp3"
            Sources.Bauchamp128.raw
            [ Comment
                { language =
                    String.fromList
                        [ Char.fromCode 0
                        , Char.fromCode 0
                        , Char.fromCode 0
                        ]
                , value = "Small comment"
                }
            , Comment
                { language = "XXX"
                , value = "Small comment"
                }
            , ContentType "R&B"
            , Year "2042"
            , RecordingTime "2042"
            , TrackNumber "128"
            , Album "Loop Mania Sampler Pack"
            , Title "Tom kick and noise"
            , LeadArtist "Bauchamp"
            ]
        , tagsTest "Celeste - Prologue"
            Sources.LenaRaine_Celeste_Prologue.raw
            [ Title "Prologue"
            , LeadArtist "Lena Raine"
            , TrackNumber "1"
            , Album "Celeste Original Soundtrack"
            , UnknownFrame { id = "APIC", raw = Bytes.Encode.encode (Bytes.Encode.sequence []) }
            , Year "2018"
            , Comment { language = "eng", value = "Visit http://radicaldreamland.bandcamp.com" }
            , Band "Lena Raine"
            ]
        ]


tagsTest : String -> String -> List Frame -> Test
tagsTest name raw expectedFrames =
    test name <|
        \_ ->
            let
                maybeFrames =
                    raw
                        |> String.split "\n"
                        |> String.concat
                        |> String.trim
                        |> Hex.Convert.toBytes
                        |> Maybe.withDefault emptyBytes
                        |> ID3.tryReadV2

                extractUnknownId frame =
                    case frame of
                        UnknownFrame { id } ->
                            Just id

                        _ ->
                            Nothing
            in
            case maybeFrames of
                Err e ->
                    Expect.fail e

                Ok { frames } ->
                    Expect.all
                        [ \_ ->
                            frames
                                |> List.filter
                                    (\frame -> extractUnknownId frame == Nothing)
                                |> Expect.equal
                                    (expectedFrames
                                        |> List.filter
                                            (\frame -> extractUnknownId frame == Nothing)
                                    )
                        , \_ ->
                            frames
                                |> List.filterMap extractUnknownId
                                |> Expect.equal
                                    (expectedFrames
                                        |> List.filterMap extractUnknownId
                                    )
                        ]
                        ()


emptyBytes : Bytes
emptyBytes =
    Bytes.Encode.encode (Bytes.Encode.sequence [])
