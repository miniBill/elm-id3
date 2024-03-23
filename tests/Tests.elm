module Tests exposing (suite)

import Bytes exposing (Bytes)
import Bytes.Encode
import Expect
import Hex.Convert
import ID3 exposing (Frame(..))
import Sources.Bauchamp128
import Sources.LenaRaine_Celeste_Prologue
import Sources.Misc
import Sources.Violin
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Main test suite"
        [ tagsTest "Bauchamp128.mp3"
            Sources.Bauchamp128.raw
            [ Comment
                { language = "\u{0000}\u{0000}\u{0000}"
                , description = ""
                , value = "Small comment"
                }
            , Comment
                { language = "XXX"
                , description = ""
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
            , apicTag
            , Year "2018"
            , Comment
                { language = "eng"
                , description = ""
                , value = "Visit http://radicaldreamland.bandcamp.com"
                }
            , Band "Lena Raine"
            ]
        , tagsTest "Violin"
            Sources.Violin.raw
            [ UserDefinedTextInformation
                { description = "comment"
                , value = "https://www.youtube.com/watch?v=xJeBz3HxGsI"
                }
            , LeadArtist "Secession Studios"
            , Title "Powerful Massive And Dramatic Neo Classical Violin Music - The Demand of Man"
            , RecordingTime "20170117"
            , UserDefinedTextInformation
                { description = "description"
                , value = "From my album - The Untold\nBandcamp ► http://bit.do/Untold_Bandcamp\nSpotify ► http://bit.do/Untold_Spotify\nItunes ► http://bit.do/Untold_Itunes\n\nSecession Studios:\nWebsite      ► http://secessionstudios.com/\nBandcamp ► http://thesecession.bandcamp.com/music\nTwitter        ► https://twitter.com/thesecession\nInstagram   ► https://instagram.com/thesecession/\nFacebook    ► http://www.facebook.com/Secession.Studios"
                }
            , UserDefinedTextInformation
                { description = "synopsis"
                , value = "From my album - The Untold\nBandcamp ► http://bit.do/Untold_Bandcamp\nSpotify ► http://bit.do/Untold_Spotify\nItunes ► http://bit.do/Untold_Itunes\n\nSecession Studios:\nWebsite      ► http://secessionstudios.com/\nBandcamp ► http://thesecession.bandcamp.com/music\nTwitter        ► https://twitter.com/thesecession\nInstagram   ► https://instagram.com/thesecession/\nFacebook    ► http://www.facebook.com/Secession.Studios"
                }
            , UserDefinedTextInformation
                { description = "purl"
                , value = "https://www.youtube.com/watch?v=xJeBz3HxGsI"
                }
            , SoftwareHardwareAndSettings "Lavf60.3.100"
            , apicTag
            ]
        , tagsTest "Misc"
            Sources.Misc.bytes
            [ Title "title" ]
        , tagsTest "Misc - \"unsupported\""
            Sources.Misc.bytesUnsupported
            [ Title "title", RecordingTime "2024", Year "2024" ]
        , tagsTest "Misc - v2.2"
            Sources.Misc.bytesv2
            []
        ]


apicTag : Frame
apicTag =
    UnknownFrame { id = "APIC", raw = Bytes.Encode.encode (Bytes.Encode.sequence []) }


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
