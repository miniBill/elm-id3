module Example exposing (suite)

import Expect
import ID3 exposing (Frame(..))
import Sources.Bauchamp128
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Bauchamp128.mp3"
        [ test "Getting tags is successfull" <|
            \_ ->
                let
                    frames : List Frame
                    frames =
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
                in
                Sources.Bauchamp128.bytes
                    |> ID3.tryReadV2
                    |> Expect.equal (Ok { frames = frames })
        ]
