module Tests exposing (bauchamp128, celestePrologue, encoding, geobSerato, githubIssue60, githubIssue73, short, violin)

import Bytes exposing (Bytes)
import Bytes.Encode
import Expect
import Hex.Convert
import ID3 exposing (Frame(..))
import Sources.Bauchamp128
import Sources.Encoding
import Sources.Geob_serato
import Sources.Github_issue_60
import Sources.Github_issue_73
import Sources.LenaRaine_CelesteOriginalSoundtrack_01Prologue
import Sources.Short
import Sources.Violin
import Test exposing (Test, test)


bauchamp128 : Test
bauchamp128 =
    tagsTest "Bauchamp128.mp3"
        Sources.Bauchamp128.raw
        [ Comment
            { language = threeZeroes
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


threeZeroes : String
threeZeroes =
    "\u{0000}\u{0000}\u{0000}"


encoding : Test
encoding =
    tagsTest "Encoding"
        Sources.Encoding.raw
        [ UserDefinedTextInformation { description = "latin1", value = "a latin1 string with £ sign" }
        , UserDefinedTextInformation { description = "utf8", value = "a utf8 string with € sign" }
        , UserDefinedTextInformation { description = "utf16be", value = "a utf16be string with € sign" }
        , UserDefinedTextInformation { description = "utf16", value = "a utf16 string with € sign (le bom)" }
        , UserDefinedTextInformation { description = "utf16bom", value = "a utf16 string with € sign (be bom)" }
        ]


geobSerato : Test
geobSerato =
    tagsTest "geob serato"
        Sources.Geob_serato.raw
        [ UserDefinedTextInformation { description = "SERATO_PLAYCOUNT", value = "0" }
        , geobTag
        , geobTag
        , geobTag
        , geobTag
        , geobTag
        , geobTag
        , geobTag
        , geobTag
        , geobTag
        , geobTag
        , geobTag
        , geobTag
        , geobTag
        , geobTag
        ]



-- These issues come from rust's id3 package github, not from elm-id3's one


githubIssue60 : Test
githubIssue60 =
    tagsTest "GH #60"
        Sources.Github_issue_60.raw
        [ UserDefinedTextInformation { description = "Encoded by", value = "Jamendo : http://www.jamendo.com | LAME" }
        , UserDefinedTextInformation { description = "Publisher", value = "Jamendo" }
        , LeadArtist "Christiaan Bakker"
        , Album "Echoes From The Past"
        , Title "Elevator Music Attempt #1"
        , UserDefinedTextInformation { description = "Copyright message", value = "2011-09-07T15:11:12+01:00 Christiaan Bakker. Licensed to the public under http://creativecommons.org/licenses/by/3.0/ verify at http://www.jamendo.comalbum/98147/" }
        , UserDefinedTextInformation { description = "Release time", value = "2011" }
        , ContentType "(255)"
        , UserDefinedTextInformation { description = "Tagging time", value = "2012-01-15T07:38:57" }
        , Comment { description = "", language = threeZeroes, value = "http://www.jamendo.com Attribution 3.0 " }
        , Comment { description = "", language = "XXX", value = "http://www.jamendo.com Attribution 3.0 " }
        ]


githubIssue73 : Test
githubIssue73 =
    tagsTest "GH #73"
        Sources.Github_issue_73.raw
        [ TrackNumber "9"
        , Title "TEST TITLE"
        , Album "TEST ALBUM"
        , RecordingTime "2016"
        , LeadArtist "TEST ARTIST"
        , Band "TEST ALBUM ARTIST"
        , ContentType "TEST GENRE"
        , Composer "TEST COMPOSER"
        , Lyricist "TEST LYRICIST"
        , UserDefinedTextInformation { description = "Category", value = "TEST CATEGORY" }
        , Publisher "TEST LABEL"
        ]


celestePrologue : Test
celestePrologue =
    tagsTest "Celeste - Prologue"
        Sources.LenaRaine_CelesteOriginalSoundtrack_01Prologue.raw
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


violin : Test
violin =
    tagsTest "Violin"
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


short : Test
short =
    tagsTest "Short"
        Sources.Short.raw
        [ Title "title" ]


apicTag : Frame
apicTag =
    UnknownFrame { id = "APIC", raw = Bytes.Encode.encode (Bytes.Encode.sequence []) }


geobTag : Frame
geobTag =
    UnknownFrame { id = "GEOB", raw = Bytes.Encode.encode (Bytes.Encode.sequence []) }


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
