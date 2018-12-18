-module(rtmp_flv).
-behaviour(gen_server).

%% API
-export([start/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("rtmp.hrl").

%% FLV header
-define(FLV_HEADER_LENGTH, 9).
-define(FLV_HEAD_SIG, <<70,76,86>>).
-define(FLV_HEAD_OFFSET, <<0,0,0,9>>).
-define(FLV_PREV_TAG_SIZE_LENGTH, 4).
-define(FLV_TAG_HEADER_LENGTH, 11).

%% FLV tag
-define(FLV_TAG_TYPE_AUDIO, 8).
-define(FLV_TAG_TYPE_VIDEO, 9).
-define(FLV_TAG_TYPE_META, 18).

%% FLV audio
-define(FLV_AUDIO_TYPE_MONO, 0).
-define(FLV_AUDIO_TYPE_STEREO, 1).
-define(FLV_AUDIO_SIZE_8BIT, 0).
-define(FLV_AUDIO_SIZE_16BIT, 1).
-define(FLV_AUDIO_RATE_5_5, 0).
-define(FLV_AUDIO_RATE_11, 1).
-define(FLV_AUDIO_RATE_22, 2).
-define(FLV_AUDIO_RATE_44, 3).
-define(FLV_AUDIO_FORMAT_UNCOMPRESSED, 0).
-define(FLV_AUDIO_FORMAT_ADPCM, 1).
-define(FLV_AUDIO_FORMAT_MP3, 2).
-define(FLV_AUDIO_FORMAT_NELLYMOSER8, 5).
-define(FLV_AUDIO_FORMAT_NELLYMOSER, 6).

%% FLV video
-define(FLV_VIDEO_CODEC_SORENSEN, 2).
-define(FLV_VIDEO_CODEC_SCREENVIDEO, 3).
-define(FLV_VIDEO_CODEC_ON2VP6, 4).
-define(FLV_VIDEO_CODEC_ON2VP6_ALPHA, 5).
-define(FLV_VIDEO_CODEC_SCREENVIDEO2, 6).
-define(FLV_VIDEO_FRAME_TYPE_KEYFRAME, 1).
-define(FLV_VIDEO_FRAME_TYPEINTER_FRAME, 2).
-define(FLV_VIDEO_FRAME_TYPEDISP_INTER_FRAME, 3).

-record(flv_header, {version = 1,
                     audio   = 0,
                     video   = 0}).

-record(flv_tag, {prev_tag_size = undefined,
                  type          = undefined,
                  body_length   = undefined,
                  timestamp     = undefined,
                  timestamp_ext = undefined,
                  timestamp_abs = undefined,
                  streamid      = undefined,
                  pos           = undefined,
                  nextpos       = undefined,
                  body          = <<>>,
                  codec_id 	    = undefined,
                  frame_type    = undefined,
                  sound_type	= undefined,
                  sound_size	= undefined,
                  sound_rate	= undefined,
                  sound_format	= undefined,
                  height		= undefined,
                  width		    = undefined,
                  amf_data      = undefined}).

-record(state, {encoder,
                stream_id,
                iodev,
                flv_header,
                flv_tag}).

%%%===================================================================
%%% API
%%%===================================================================
start(Encoder, StreamID) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Encoder, StreamID], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([Encoder, StreamID]) ->
    FlvFile = "/tmp/test.flv",
    {ok, IoDev} = file:open(FlvFile, [read]),
    {ok, FirstFlvTagPtr, FlvHeader} = read_header(IoDev),
    {ok, FlvTag} = read_tag(IoDev, FirstFlvTagPtr),
    erlang:send_after(0, self(), send),
    {ok, #state{encoder = Encoder,
                stream_id = StreamID,
                iodev = IoDev,
                flv_header = FlvHeader,
                flv_tag = FlvTag}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(send, #state{encoder = Encoder,
                         stream_id = StreamID,
                         iodev = IoDev,
                         flv_tag = FlvTag} = State) ->
    Message = case FlvTag#flv_tag.type of
                  ?FLV_TAG_TYPE_AUDIO ->
                      {?RTMP_MSG_AUDIO, FlvTag#flv_tag.body};
                  ?FLV_TAG_TYPE_VIDEO ->
                      {?RTMP_MSG_VIDEO, FlvTag#flv_tag.body};
                  ?FLV_TAG_TYPE_META ->
                      {?RTMP_MSG_DATA_AMF0_BIN, FlvTag#flv_tag.amf_data}
              end,
    rtmp_encode:send_message(Encoder, StreamID, Message),
    case read_tag(IoDev, FlvTag#flv_tag.nextpos) of
        {ok, NextFlvTag} ->
            erlang:send_after(10, self(), send),
            {noreply, State#state{flv_tag = NextFlvTag}};
        Else ->
            ?LOG_ERROR("Error: ~p", [Else]),
            {noreply, State}
        end;
handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, #state{iodev = IoDev} = State) ->
    file:close(IoDev),
    ?LOG_ERROR("rtmp_flv ~p terminater: ~p", [self(), Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
read_header(IoDev) ->
    case file:read(IoDev, ?FLV_HEADER_LENGTH) of
        {ok, Data} ->
            {ok, iolist_size(Data), header(Data)};
        eof ->
            {error, unexpected_eof};
        {error, Reason} ->
            {error, Reason}
    end.

read_tag(IoDev, Pos) ->
    case file:pread(IoDev,
                    Pos,
                    ?FLV_PREV_TAG_SIZE_LENGTH + ?FLV_TAG_HEADER_LENGTH) of
        {ok, IoList} ->
            case iolist_to_binary(IoList) of
                <<PrevTagSize:32/integer,
                  Type:8,
                  BodyLength:24,
                  TimeStamp:24,
                  TimeStampExt:8,
                  StreamId:24>> ->
                    case file:pread(IoDev,
                                    Pos + ?FLV_PREV_TAG_SIZE_LENGTH + ?FLV_TAG_HEADER_LENGTH,
                                    BodyLength) of
                        {ok, IoList2} ->
                            <<TimeStampAbs:32>> = <<TimeStampExt:8, TimeStamp:24>>,
                            TagData = #flv_tag{prev_tag_size = PrevTagSize,
                                               type = Type,
                                               body_length = BodyLength,
                                               timestamp_abs = TimeStampAbs,
                                               streamid = StreamId,
                                               pos = Pos,
                                               nextpos = Pos + ?FLV_PREV_TAG_SIZE_LENGTH + ?FLV_TAG_HEADER_LENGTH + BodyLength,
                                               body  = iolist_to_binary(IoList2)},
                            case Type of
                                8 ->
                                    {SoundType,
                                     SoundSize,
                                     SoundRate,
                                     SoundFormat} = extractAudioHeader(IoDev, Pos),
                                    {ok, TagData#flv_tag{sound_type = SoundType,
                                                         sound_size = SoundSize,
                                                         sound_rate = SoundRate,
                                                         sound_format = SoundFormat}};
                                9 ->
                                    {FrameType,
                                     CodecID,
                                     Width,
                                     Height} = extractVideoHeader(IoDev, Pos),
                                    {ok, TagData#flv_tag{frame_type = FrameType,
                                                         codec_id = CodecID,
                                                         width = Width,
                                                         height = Height}};
                                18 ->
                                    %% === @TODO: Bugfix
                                    %%AmfData = ems_amf:decode(iolist_to_binary(IoList2)),
                                    AmfData = iolist_to_binary(IoList2),
                                    {ok, TagData#flv_tag{amf_data = AmfData}}

                            end;
                        eof ->
                            {ok, done};
                        {error, Reason} ->
                            {error, Reason}
                    end;
                _ ->
                    %% === @TODO: Bugfix
                    {ok, done}
            end;
        eof ->
            {error, unexpected_eof};
        {error, Reason} ->
            {error, Reason}
    end.

read_tag(<<PrevTagSize:32/integer,
           Type:8,
           BodyLength:24,
           TimeStamp:24,
           TimeStampExt:8,
           StreamId:24,
           Body/binary>>) ->
    <<TimeStampAbs:32>> = <<TimeStampExt:8, TimeStamp:24>>,
    case Type of
        18 ->
            {ok, #flv_tag{prev_tag_size = PrevTagSize,
                          type = Type,
                          body_length = BodyLength,
                          timestamp_abs = TimeStampAbs,
                          streamid = StreamId,
                          body = Body}};
        _ ->
            error
    end.

getWidthHeight(IoDev, Pos, CodecID)->
    case CodecID of
        ?FLV_VIDEO_CODEC_SORENSEN ->
            decodeSorensen(IoDev, Pos);
        ?FLV_VIDEO_CODEC_SCREENVIDEO ->
            decodeScreenVideo(IoDev, Pos);
        ?FLV_VIDEO_CODEC_ON2VP6 ->
            decodeVP6(IoDev, Pos);
        ?FLV_VIDEO_CODEC_ON2VP6_ALPHA ->
            decodeVP6(IoDev, Pos);
        ?FLV_VIDEO_CODEC_SCREENVIDEO2 ->
            {undefined, undefined}
    end.

extractVideoHeader(IoDev, Pos) ->
    case file:pread(IoDev,
                    Pos + ?FLV_PREV_TAG_SIZE_LENGTH + ?FLV_TAG_HEADER_LENGTH,
                    1) of
        {ok, IoList3} ->
            <<FrameType:4, CodecID:4>> = iolist_to_binary(IoList3),
            {Width, Height} = getWidthHeight(IoDev, Pos, CodecID),
            {FrameType, CodecID, Width, Height};
        eof ->
            {ok, done};
        {error, Reason} ->
            {error, Reason}
    end.

decodeScreenVideo(IoDev, Pos) ->
    {ok, IoList4} = file:pread(IoDev, Pos + ?FLV_PREV_TAG_SIZE_LENGTH + ?FLV_TAG_HEADER_LENGTH + 1, 4),
    <<_Offset:4, Width:12, Height:12, _Rest:4>> = iolist_to_binary(IoList4),
    {Width, Height}.

decodeSorensen(IoDev, Pos) ->
    {ok, IoList4} = file:pread(IoDev,
                               Pos + ?FLV_PREV_TAG_SIZE_LENGTH + ?FLV_TAG_HEADER_LENGTH + 1,
                               9),
    <<_Offset:30, Info:3, _Rest:39>> = iolist_to_binary(IoList4),
    case Info of
        0 ->
            <<_Offset1:30, _Info1:3, Width1:8, Height1:8, _Rest1:23>> = iolist_to_binary(IoList4),
            {Width1, Height1};
        1 ->
            <<_Offset2:30, _Info2:3, Width2:16, Height2:16, _Rest2:7>>  = iolist_to_binary(IoList4),
            {Width2, Height2};
        2 ->
            {352, 288};
        3 ->
            {176, 144};
        4 ->
            {128, 96};
        5 ->
            {320, 240};
        6 ->
            {160, 120}
    end.

decodeVP6(IoDev, Pos)->
    {ok, IoList4} = file:pread(IoDev,
                               Pos + ?FLV_PREV_TAG_SIZE_LENGTH + ?FLV_TAG_HEADER_LENGTH + 1,
                               7),
    <<HeightHelper:4, WidthHelper:4, _Offset:24, Width:8, Height:8>> = iolist_to_binary(IoList4),
    {Width*16-WidthHelper, Height*16-HeightHelper}.

extractAudioHeader(IoDev, Pos) ->
    {ok,IoList3} = file:pread(IoDev,
                              Pos + ?FLV_PREV_TAG_SIZE_LENGTH + ?FLV_TAG_HEADER_LENGTH,
                              1),
    <<SoundFormat:4, SoundRate:2, SoundSize:1, SoundType:1>> = iolist_to_binary(IoList3),
    {SoundType, SoundSize, SoundRate, SoundFormat}.

encodeTag(#flv_tag{} = Tag, Body) ->
    TimeStampAbs = Tag#flv_tag.timestamp_abs,
    PrevTagSize = Tag#flv_tag.prev_tag_size,
    Type = Tag#flv_tag.type,
    BodyLength = Tag#flv_tag.body_length,
    StreamId = Tag#flv_tag.streamid,
    <<PrevTagSize:32,
      Type:8,
      BodyLength:24,
      TimeStampAbs:32,
      StreamId:24,
      Body/binary>>.

header(#flv_header{version = Version,
                   audio = Audio,
                   video = Video}) ->
    Reserved = 0,
    Offset = 9,
    PrevTag = 0,
    <<70,76,86,
      Version:8,
      Reserved:5,
      Audio:1,
      Reserved:1,
      Video:1,
      Offset:32,
      PrevTag:32>>;

header(<<70,76,86,
         Ver:8,
         _:5,
         Audio:1,
         _:1,
         Video:1,
         0,0,0,9>>) ->
    #flv_header{version = Ver, audio = Audio, video = Video};

header(IoList) ->
    header(iolist_to_binary(IoList)).

to_tag(Message,
       FullTimeStamp,
       Type,
       StreamId,
       PrevTimeStamp) ->
    BodyLength = size(Message),
    {TimeStampExt, TimeStamp} =
        case PrevTimeStamp of
            <<TimeStampExt1:8, TimeStamp1:32>> ->
                {TimeStampExt1, TimeStamp1};
            _ ->
                {0, PrevTimeStamp}
        end,
    PrevTagSize = size(Message) + 11,
    {<<Type:8,
       BodyLength:24,
       TimeStamp:24,
       TimeStampExt:8,
       StreamId:24,
       Message/binary,
       PrevTagSize:32>>,
     FullTimeStamp + PrevTimeStamp}.

parse_meta(Bin) ->
    %% === @TODO
    Bin.

write_terms(Filename, Term) ->
    Format = fun(E) ->
                     io_lib:format("~tp.~n", [E])
             end,
    file:write_file(Filename, lists:map(Format, Term)).
