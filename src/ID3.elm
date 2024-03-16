module ID3 exposing (Frame(..), ID3v2, tryReadV2)

import Bitwise
import Bytes exposing (Bytes)
import Bytes.Encode
import Bytes.Parser as Parser exposing (Parser)
import Dict exposing (Dict)
import Hex.Convert
import Iso8859.Part1
import String.UTF16


type alias ID3v2 =
    { frames : List Frame }


type Frame
    = Comment
        { language : String
        , description : String
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
    | UserDefinedTextInformation
        { description : String
        , value : String
        }
    | UnknownFrame
        { id : String
        , raw : Bytes
        }


type alias Header =
    { flags : Flags
    , version : Version

    -- size does not include the header itself, nor the optional footer
    , size : Int

    -- innerSize excludes the extended header too
    , innerSize : Int
    }


type alias Flags =
    { unsynchronization : Bool
    , extendedHeader : Bool
    , footerPresent : Bool
    }


type Version
    = ID3v2_3
    | ID3v2_4


type alias FrameHeader =
    { id : String
    , size : Int
    , flags : ( Int, Int )
    }


type Context
    = HeaderContext
    | FramesContext
    | FrameNameContext
    | FrameHeaderContext
    | FrameSizeContext String
    | FlagsContext String
    | ExtendedHeaderContext
    | FrameContext
    | InnerFrameContext String
    | COMMLanguageContext
    | COMMValueContext
    | FirstStringContext
    | SecondStringContext


log : String -> a -> a
log =
    -- always identity
    Debug.log


tryReadV2 : Bytes -> Result String ID3v2
tryReadV2 bytes =
    Parser.run v2Parser bytes
        |> Result.mapError errorToString


errorToString : Parser.Error Context String -> String
errorToString error =
    case error of
        Parser.Custom _ e ->
            e

        Parser.InContext context child ->
            contextToString context ++ " > " ++ errorToString child

        Parser.OutOfBounds { at, bytes } ->
            "Out of bounds - reading " ++ String.fromInt bytes ++ " at " ++ String.fromInt at

        Parser.BadOneOf _ children ->
            children
                |> List.map errorToString
                |> String.join " or "


contextToString : { label : Context, start : Int } -> String
contextToString { label } =
    case label of
        HeaderContext ->
            "Header"

        FramesContext ->
            "Frames"

        FrameContext ->
            "Frame"

        FrameHeaderContext ->
            "Frame header"

        FrameNameContext ->
            "Frame name"

        FrameSizeContext id ->
            "[" ++ id ++ "] frame size"

        FlagsContext id ->
            "[" ++ id ++ "] frame flags"

        ExtendedHeaderContext ->
            "Extended header"

        InnerFrameContext id ->
            "[" ++ id ++ "] content"

        COMMLanguageContext ->
            "Language"

        COMMValueContext ->
            "Value"

        FirstStringContext ->
            "First string"

        SecondStringContext ->
            "Second string"


v2Parser : Parser Context String ID3v2
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
                                    frameParser header.version
                                        |> Parser.map
                                            (\maybeFrame ->
                                                case maybeFrame of
                                                    Nothing ->
                                                        acc
                                                            |> List.reverse
                                                            |> Parser.Done

                                                    Just ( frame, frameLength ) ->
                                                        ( remaining - frameLength
                                                        , frame :: acc
                                                        )
                                                            |> Parser.Loop
                                            )
                            )
                            ( header.innerSize, [] )
                        )
                    |> Parser.inContext FramesContext
            )



------------------------------------------------------------------------------------------------------------
--                                                                                                        --
--                                                 Header                                                 --
--                                                                                                        --
------------------------------------------------------------------------------------------------------------


headerParser : Parser Context String Header
headerParser =
    Parser.succeed (\version flags size -> ( version, flags, size ))
        |> Parser.ignore identifierParser
        |> Parser.keep majorVersionParser
        |> Parser.ignore minorVersionParser
        |> Parser.keep flagsParser
        |> Parser.keep syncsafeInt32Parser
        |> Parser.map (log "header")
        |> Parser.andThen
            (\( version, flags, size ) ->
                if flags.extendedHeader then
                    Parser.succeed
                        (\extended ->
                            { version = version
                            , flags = flags
                            , size = size
                            , innerSize = size - extended.size
                            }
                        )
                        |> Parser.keep extendedHeader

                else
                    Parser.succeed
                        { version = version
                        , flags = flags
                        , size = size
                        , innerSize = size
                        }
            )
        |> Parser.inContext HeaderContext


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


majorVersionParser : Parser context String Version
majorVersionParser =
    Parser.unsignedInt8
        |> validate
            (\major ->
                case major of
                    3 ->
                        Ok ID3v2_3

                    4 ->
                        Ok ID3v2_4

                    _ ->
                        Err <| "This library currently supports ID3v2.3 and ID3v2.4 only, found ID3v2." ++ String.fromInt major
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


extendedHeader : Parser Context String { size : Int }
extendedHeader =
    syncsafeInt32Parser
        |> Parser.andThen
            (\size ->
                Parser.succeed { size = log "extendedHeader size" size }
                    |> Parser.skip (size - 4)
            )
        |> Parser.inContext ExtendedHeaderContext



------------------------------------------------------------------------------------------------------------
--                                                                                                        --
--                                                 Frame                                                  --
--                                                                                                        --
------------------------------------------------------------------------------------------------------------


frameParser : Version -> Parser Context String (Maybe ( Frame, Int ))
frameParser version =
    frameHeaderParser version
        |> Parser.andThen
            (\maybeHeader ->
                case maybeHeader of
                    Nothing ->
                        Parser.succeed Nothing

                    Just { id, size, flags } ->
                        let
                            ( _, lowerFlag ) =
                                flags

                            innerParser : Parser Context String Frame
                            innerParser =
                                case version of
                                    ID3v2_3 ->
                                        if hasBit 7 lowerFlag then
                                            Parser.fail "Compressed frames are not supported yet"

                                        else if hasBit 6 lowerFlag then
                                            Parser.fail "Encrypted frames are not supported yet"

                                        else
                                            innerFrameParser id size

                                    ID3v2_4 ->
                                        if hasBit 3 lowerFlag then
                                            Parser.fail "Compressed frames are not supported yet"

                                        else if hasBit 2 lowerFlag then
                                            Parser.fail "Encrypted frames are not supported yet"

                                        else if hasBit 1 lowerFlag then
                                            Parser.fail "Unsynchronisation is not supported yet"

                                        else if hasBit 0 lowerFlag then
                                            Parser.fail "Data length indicator not supported yet"

                                        else
                                            innerFrameParser id size
                        in
                        innerParser
                            |> Parser.map (\frame -> log "Parsed frame" frame)
                            |> Parser.map (\frame -> Just ( frame, size + 10 ))
            )
        |> Parser.inContext FrameContext


zeroId : String
zeroId =
    String.fromList (List.repeat 4 (Char.fromCode 0))


frameHeaderParser : Version -> Parser Context String (Maybe FrameHeader)
frameHeaderParser version =
    (Parser.string 4
        |> validate
            (\id ->
                if id == zeroId then
                    Ok Nothing

                else if String.all (\c -> Char.isUpper c || Char.isDigit c) id then
                    Ok (Just id)

                else
                    Err <| "Unexpected frame ID: " ++ id
            )
        |> Parser.inContext FrameNameContext
    )
        |> Parser.andThen
            (\maybeId ->
                case maybeId of
                    Nothing ->
                        Parser.succeed Nothing

                    Just id ->
                        Parser.succeed
                            (\size flags ->
                                log "Parsing frame"
                                    { id = id
                                    , size = size
                                    , flags = flags
                                    }
                                    |> Just
                            )
                            |> Parser.keep
                                (unsignedInt32 version
                                    |> Parser.inContext (FrameSizeContext id)
                                )
                            |> Parser.keep
                                (Parser.map2 Tuple.pair syncsafeByteParser syncsafeByteParser
                                    |> Parser.inContext (FlagsContext id)
                                )
            )
        |> Parser.inContext FrameHeaderContext


innerFrameParser : String -> Int -> Parser Context String Frame
innerFrameParser id size =
    (case Dict.get id textInformationFrames of
        Just ctor ->
            Parser.map ctor (prefixedStringParser size)

        Nothing ->
            case id of
                "COMM" ->
                    stringKindParser
                        |> Parser.andThen
                            (\kind ->
                                Parser.succeed
                                    (\language ( description, value ) ->
                                        Comment
                                            { language = language
                                            , description = description
                                            , value = value
                                            }
                                    )
                                    |> Parser.keep
                                        (Parser.string 3
                                            |> Parser.inContext COMMLanguageContext
                                        )
                                    |> Parser.keep
                                        (twoStringsParserOfKind kind (size - 4)
                                            |> Parser.inContext COMMValueContext
                                        )
                            )

                "TXXX" ->
                    stringKindParser
                        |> Parser.andThen
                            (\kind ->
                                Parser.succeed
                                    (\( description, value ) ->
                                        UserDefinedTextInformation
                                            { description = description
                                            , value = value
                                            }
                                    )
                                    |> Parser.keep (twoStringsParserOfKind kind (size - 1))
                            )

                _ ->
                    Parser.bytes size
                        |> Parser.map
                            (\raw ->
                                { id = id
                                , raw = raw
                                }
                                    |> UnknownFrame
                            )
    )
        |> Parser.inContext (InnerFrameContext id)


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
    stringKindParser
        |> Parser.andThen
            (\kind -> stringParserOfKind kind (length - 1))


type StringKind
    = Iso8859
    | UTF16
    | UTF16BE
    | UTF8


stringKindParser : Parser context String StringKind
stringKindParser =
    Parser.unsignedInt8
        |> validate
            (\rawKind ->
                case rawKind of
                    0 ->
                        Ok Iso8859

                    1 ->
                        Ok UTF16

                    2 ->
                        Ok UTF16BE

                    3 ->
                        Ok UTF8

                    _ ->
                        Err <| "Unexpected encoding byte for string: " ++ String.fromInt rawKind
            )


stringParserOfKind : StringKind -> Int -> Parser context String String
stringParserOfKind kind length =
    (case kind of
        Iso8859 ->
            Parser.bytes length
                |> Parser.andThen
                    (\bytes ->
                        case Iso8859.Part1.toString bytes of
                            Just decoded ->
                                Parser.succeed decoded

                            Nothing ->
                                Parser.fail <| "Failed to parse " ++ Hex.Convert.toString bytes ++ " as ISO8859-1"
                    )

        UTF16 ->
            Parser.succeed Tuple.pair
                |> Parser.keep (Parser.unsignedInt16 Bytes.LE)
                |> Parser.keep (Parser.bytes <| length - 2)
                |> Parser.andThen
                    (\( bom, bytes ) ->
                        let
                            endianess : Bytes.Endianness
                            endianess =
                                if bom == 0xFEFF then
                                    Bytes.LE

                                else
                                    Bytes.BE
                        in
                        case String.UTF16.toString endianess bytes of
                            Just decoded ->
                                Parser.succeed decoded

                            Nothing ->
                                Parser.fail <| "Failed to parse " ++ Hex.Convert.toString bytes ++ " as UTF16"
                    )

        UTF16BE ->
            Parser.fail "UTF-16 [BE] is not supported for strings yet"

        UTF8 ->
            Parser.string length
    )
        |> Parser.map
            (\str ->
                -- There are subtle differences in frames, versions, ...
                -- We could spend a lot of time getting it right, or we can just do this
                if String.endsWith "\u{0000}" str then
                    String.dropRight 1 str

                else
                    str
            )


twoStringsParserOfKind : StringKind -> Int -> Parser Context String ( String, String )
twoStringsParserOfKind kind length =
    let
        zeroSize : Int
        zeroSize =
            case kind of
                Iso8859 ->
                    1

                UTF16 ->
                    2

                UTF16BE ->
                    2

                UTF8 ->
                    1

        zeroParser : Parser context error Int
        zeroParser =
            if zeroSize == 1 then
                Parser.unsignedInt8

            else
                Parser.unsignedInt16 Bytes.BE

        findZero : Parser context error Int
        findZero =
            Parser.position
                |> Parser.andThen
                    (\position ->
                        Parser.randomAccess { offset = 0, relativeTo = position } <|
                            Parser.loop
                                (\offset ->
                                    zeroParser
                                        |> Parser.map
                                            (\int ->
                                                if int == 0 || offset == length then
                                                    Parser.Done <| Debug.log "zero at offset" <| offset * zeroSize

                                                else
                                                    Parser.Loop (offset + 1)
                                            )
                                )
                                0
                    )
    in
    findZero
        |> Parser.andThen
            (\firstZero ->
                Parser.succeed Tuple.pair
                    |> Parser.keep
                        (stringParserOfKind kind firstZero
                            |> Parser.inContext FirstStringContext
                        )
                    |> Parser.skip zeroSize
                    |> Parser.keep
                        (stringParserOfKind kind (length - firstZero - zeroSize)
                            |> Parser.inContext SecondStringContext
                        )
            )



------------------------------------------------------------------------------------------------------------
--                                                                                                        --
--                                                 Utils                                                  --
--                                                                                                        --
------------------------------------------------------------------------------------------------------------


unsignedInt32 : Version -> Parser Context String Int
unsignedInt32 version =
    case version of
        ID3v2_3 ->
            Parser.unsignedInt32 Bytes.BE

        ID3v2_4 ->
            syncsafeInt32Parser


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
                    let
                        byteString : String
                        byteString =
                            Hex.Convert.toString <| Bytes.Encode.encode <| Bytes.Encode.unsignedInt8 byte
                    in
                    Err <| "Byte should have been less than 0x80: " ++ byteString
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
