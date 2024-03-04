module ID3 exposing (Frame(..), ID3v2, tryReadV2)

import Bitwise
import Bytes exposing (Bytes)
import Bytes.Parser as Parser exposing (Parser)
import Dict exposing (Dict)
import Hex.Convert
import Iso8859.Part1


type alias ID3v2 =
    { frames : List Frame }


type Frame
    = Comment
        { language : String
        , value : String
        }
    | ContentType String
    | Year String
    | RecordingTime String
    | TrackNumber String
    | ContentGroupDescription String
    | Title String
    | Subtitle String
    | Album String
    | SetSubtitle String
    | Band String
    | Conductor String
    | OriginalArtist String
    | Lyricist String
    | OriginalLyricist String
    | Composer String
    | MusicianCreditsList String
    | EncodedBy String
    | BPM String
    | Length String
    | InitialKey String
    | Language String
    | FileType String
    | MediaType String
    | Mood String
    | ProducedNotice String
    | Publisher String
    | InternetRadioStationOwner String
    | OriginalFilename String
    | PlaylistDelay String
    | EncodingTime String
    | OriginalReleaseTime String
    | ReleaseTime String
    | TaggingTime String
    | SoftwareHardwareAndSettings String
    | AlbumSortOrder String
    | PerformerSortOrder String
    | TitleSortOrder String
    | InternetRadioStationName String
    | FileOwnerLicensee String
    | CopyrightMessage String
    | InvolvedPeopleList String
    | LeadArtist String
    | InterpretedBy String
    | ISRC String
    | PartOfASet String
    | OriginalAlbum String
    | UnknownFrame
        { id : String
        , raw : String
        }


type alias Flags =
    { unsynchronization : Bool
    , extendedHeader : Bool
    , footerPresent : Bool
    }


type alias Header =
    { flags : Flags

    -- size does not include the header itself, nor the optional footer
    , size : Int

    -- innerSize excludes the extended header too
    , innerSize : Int
    }


log : String -> a -> a
log _ value =
    value


tryReadV2 : Bytes -> Result String ID3v2
tryReadV2 bytes =
    Parser.run v2Parser bytes
        |> Result.mapError errorToString


errorToString : Parser.Error context String -> String
errorToString error =
    case error of
        Parser.Custom _ e ->
            e

        Parser.InContext _ child ->
            errorToString child

        Parser.OutOfBounds _ ->
            "Out of bounds"

        Parser.BadOneOf _ children ->
            children
                |> List.map errorToString
                |> String.join " or "


v2Parser : Parser context String ID3v2
v2Parser =
    headerParser
        |> Parser.andThen
            (\header ->
                Parser.succeed (\frames -> { frames = frames })
                    |> Parser.keep
                        (Parser.loop
                            (\( remaining, acc ) ->
                                if log "remaining" remaining <= 0 then
                                    acc
                                        |> List.reverse
                                        |> Parser.Done
                                        |> Parser.succeed

                                else
                                    parseFrame
                                        |> Parser.map
                                            (\( frame, frameLength ) ->
                                                ( remaining - frameLength
                                                , frame :: acc
                                                )
                                                    |> Parser.Loop
                                            )
                            )
                            ( header.innerSize, [] )
                        )
            )



------------------------------------------------------------------------------------------------------------
--                                                                                                        --
--                                                 Header                                                 --
--                                                                                                        --
------------------------------------------------------------------------------------------------------------


headerParser : Parser context String Header
headerParser =
    Parser.succeed Tuple.pair
        |> Parser.ignore identifierParser
        |> Parser.ignore majorVersionParser
        |> Parser.ignore minorVersionParser
        |> Parser.keep flagsParser
        |> Parser.keep syncsafeInt32Parser
        |> Parser.map (log "header")
        |> Parser.andThen
            (\( flags, size ) ->
                if flags.extendedHeader then
                    Parser.succeed
                        (\extended ->
                            { flags = flags
                            , size = size
                            , innerSize = size - extended.size
                            }
                        )
                        |> Parser.keep extendedHeader

                else
                    Parser.succeed
                        { flags = flags
                        , size = size
                        , innerSize = size
                        }
            )


identifierParser : Parser context String ()
identifierParser =
    Parser.string 3
        |> validate
            (\tag ->
                if tag == "ID3" then
                    Ok ()

                else
                    Err <| "ID3 not found, found " ++ tag ++ " instead"
            )


majorVersionParser : Parser context String ()
majorVersionParser =
    Parser.unsignedInt8
        |> validate
            (\major ->
                if major == 4 then
                    Ok ()

                else
                    Err <| "This library currently supports ID3v2.4 only, found ID3v2." ++ String.fromInt major
            )


minorVersionParser : Parser context String Int
minorVersionParser =
    Parser.unsignedInt8


flagsParser : Parser context String Flags
flagsParser =
    Parser.unsignedInt8
        |> validate
            (\flags ->
                if hasBit 5 flags then
                    Err "Experimental header not supported"

                else if hasBit 7 flags then
                    Err "Unsynchronization not supported yet"

                else if hasBit 3 flags || hasBit 2 flags || hasBit 1 flags || hasBit 0 flags then
                    Err "Unknown flag set"

                else
                    { unsynchronization = hasBit 7 flags
                    , extendedHeader = hasBit 6 flags
                    , footerPresent = hasBit 4 flags
                    }
                        |> log "flags"
                        |> Ok
            )


extendedHeader : Parser context String { size : Int }
extendedHeader =
    syncsafeInt32Parser
        |> Parser.andThen
            (\size ->
                Parser.succeed { size = log "extendedHeader size" size }
                    |> Parser.skip (size - 4)
            )



------------------------------------------------------------------------------------------------------------
--                                                                                                        --
--                                                 Frame                                                  --
--                                                                                                        --
------------------------------------------------------------------------------------------------------------


parseFrame : Parser context String ( Frame, Int )
parseFrame =
    Parser.succeed
        (\id size flags ->
            log "Parsing frame"
                { id = id
                , size = size
                , flags = flags
                }
        )
        |> Parser.keep
            (Parser.string 4
                |> validate
                    (\id ->
                        if String.all (\c -> Char.isUpper c || Char.isDigit c) id then
                            Ok id

                        else
                            Err <| "Unexpected frame ID: " ++ id
                    )
            )
        |> Parser.keep syncsafeInt32Parser
        |> Parser.keep (Parser.map2 Tuple.pair syncsafeByteParser syncsafeByteParser)
        |> Parser.andThen
            (\{ id, size, flags } ->
                let
                    ( _, lowerFlag ) =
                        flags
                in
                if hasBit 3 lowerFlag then
                    Parser.fail "Compressed frames are not supported yet"

                else if hasBit 2 lowerFlag then
                    Parser.fail "Encrypted frames are not supported yet"

                else if hasBit 1 lowerFlag then
                    Parser.fail "Unsynchronisation is not supported yet"

                else if hasBit 0 lowerFlag then
                    Parser.fail "Data length indicator not supported yet"

                else
                    (case Dict.get id textInformationFrames of
                        Just ctor ->
                            Parser.map ctor (prefixedStringParser size)

                        Nothing ->
                            case id of
                                "COMM" ->
                                    Parser.succeed
                                        (\language value ->
                                            Comment
                                                { language = language
                                                , value = value
                                                }
                                        )
                                        |> Parser.keep (prefixedStringParser 4)
                                        |> Parser.keep (prefixedStringParser <| size - 4)

                                _ ->
                                    prefixedStringParser size
                                        |> Parser.map
                                            (\raw ->
                                                { id = id
                                                , raw = raw
                                                }
                                                    |> UnknownFrame
                                            )
                    )
                        |> Parser.map (\frame -> ( frame, size + 10 ))
            )
        |> Parser.map (\frame -> log "Parsed frame" frame)


textInformationFrames : Dict String (String -> Frame)
textInformationFrames =
    [ -- Identification frames
      ( "TIT1", ContentGroupDescription )
    , ( "TIT2", Title )
    , ( "TIT3", Subtitle )
    , ( "TALB", Album )
    , ( "TOAL", OriginalAlbum )
    , ( "TRCK", TrackNumber )
    , ( "TPOS", PartOfASet )
    , ( "TSST", SetSubtitle )
    , ( "TSRC", ISRC )

    -- Involved persons frames
    , ( "TPE1", LeadArtist )
    , ( "TPE2", Band )
    , ( "TPE3", Conductor )
    , ( "TPE4", InterpretedBy )
    , ( "TOPE", OriginalArtist )
    , ( "TEXT", Lyricist )
    , ( "TOLY", OriginalLyricist )
    , ( "TCOM", Composer )
    , ( "TCML", MusicianCreditsList )
    , ( "TIPL", InvolvedPeopleList )
    , ( "TENC", EncodedBy )

    -- Derived and subjective properties frames
    , ( "TBPM", BPM )
    , ( "TLEN", Length )
    , ( "TKEY", InitialKey )
    , ( "TLAN", Language )
    , ( "TCON", ContentType )
    , ( "TFLT", FileType )
    , ( "TMED", MediaType )
    , ( "TMOO", Mood )

    -- Rights and license frames
    , ( "TCOP", CopyrightMessage )
    , ( "TPRO", ProducedNotice )
    , ( "TPUB", Publisher )
    , ( "TOWN", FileOwnerLicensee )
    , ( "TRSN", InternetRadioStationName )
    , ( "TRSO", InternetRadioStationOwner )

    -- Other text frames
    , ( "TOFN", OriginalFilename )
    , ( "TDLY", PlaylistDelay )
    , ( "TDEN", EncodingTime )
    , ( "TDOR", OriginalReleaseTime )
    , ( "TDRC", RecordingTime )
    , ( "TDRL", ReleaseTime )
    , ( "TDTG", TaggingTime )
    , ( "TSSE", SoftwareHardwareAndSettings )
    , ( "TSOA", AlbumSortOrder )
    , ( "TSOP", PerformerSortOrder )
    , ( "TSOT", TitleSortOrder )

    -- Legacy compat
    , ( "TYER", Year )
    ]
        |> Dict.fromList


{-| A string with a one-byte prefix encoding. The length passed in should include the encoding byte.
-}
prefixedStringParser : Int -> Parser context String String
prefixedStringParser length =
    Parser.unsignedInt8
        |> Parser.andThen
            (\kind ->
                case kind of
                    0 ->
                        Parser.bytes (length - 1)
                            |> Parser.andThen
                                (\bytes ->
                                    case Iso8859.Part1.toString bytes of
                                        Just decoded ->
                                            Parser.succeed decoded

                                        Nothing ->
                                            Parser.fail <| "Failed to parse " ++ Hex.Convert.toString bytes ++ " as ISO8859-1"
                                )

                    1 ->
                        Parser.fail "UTF-16 is not supported for strings yet"

                    2 ->
                        Parser.fail "UTF-16 [BTE] is not supported for strings yet"

                    3 ->
                        Parser.string (length - 1)

                    _ ->
                        Parser.fail <| "Unexpected encoding byte for string: " ++ String.fromInt kind
            )



------------------------------------------------------------------------------------------------------------
--                                                                                                        --
--                                                 Utils                                                  --
--                                                                                                        --
------------------------------------------------------------------------------------------------------------


syncsafeInt32Parser : Parser context String Int
syncsafeInt32Parser =
    Parser.succeed
        (\a b c d ->
            a
                |> Bitwise.shiftLeftBy 7
                |> Bitwise.or b
                |> Bitwise.shiftLeftBy 7
                |> Bitwise.or c
                |> Bitwise.shiftLeftBy 7
                |> Bitwise.or d
        )
        |> Parser.keep syncsafeByteParser
        |> Parser.keep syncsafeByteParser
        |> Parser.keep syncsafeByteParser
        |> Parser.keep syncsafeByteParser


syncsafeByteParser : Parser context String Int
syncsafeByteParser =
    Parser.unsignedInt8
        |> validate
            (\byte ->
                if byte < 0x80 then
                    Ok byte

                else
                    Err "Byte should have been less than 0x80"
            )


hasBit : Int -> Int -> Bool
hasBit index input =
    let
        mask : Int
        mask =
            Bitwise.shiftLeftBy index 1
    in
    Bitwise.and mask input == mask


validate : (value -> Result error result) -> Parser context error value -> Parser context error result
validate validator parser =
    Parser.position
        |> Parser.andThen
            (\position ->
                parser
                    |> Parser.andThen
                        (\value ->
                            case validator value of
                                Ok result ->
                                    Parser.succeed result

                                Err e ->
                                    Parser.randomAccess { offset = 0, relativeTo = position } (Parser.fail e)
                        )
            )
