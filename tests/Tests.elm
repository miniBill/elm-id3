module Tests exposing (suite)

import Bytes exposing (Bytes)
import Bytes.Encode
import Expect
import Hex.Convert
import ID3 exposing (Frame(..))
import Sources.Bauchamp128
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
        ]


tagsTest : String -> String -> List Frame -> Test
tagsTest name raw frames =
    test name <|
        \_ ->
            raw
                |> String.split "\n"
                |> String.concat
                |> String.trim
                |> Hex.Convert.toBytes
                |> Maybe.withDefault emptyBytes
                |> ID3.tryReadV2
                |> Expect.equal (Ok { frames = frames })


emptyBytes : Bytes
emptyBytes =
    Bytes.Encode.encode (Bytes.Encode.sequence [])
