-module(rtmp_publish).

-behaviour(gen_server).

%% API
-export([get_pid/3,
         get_pid/2,
         register_viewer/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("rtmp.hrl").

-record(state, {related_to_channel :: pid(),
                actors_list = [] :: list(),
                stream_info_amf_bin = undefined:: binary(),
                key_frame = undefined :: binary()}).

%%%===================================================================
%%% API
%%%===================================================================
get_pid(viewer, StreamName) ->
    case rtmp_cluster:get_actor_pid({?PUBLISHER_ACTOR_PREFIX, StreamName}) of
        {ok, PublisherActorPID} ->
            {ok, PublisherActorPID};
        _ ->
            this_stream_is_not_published_right_now
    end.

get_pid(publisher, PublisherChannel ,StreamName) ->
    case rtmp_cluster:get_actor_pid({?PUBLISHER_ACTOR_PREFIX, StreamName}) of
        {ok, PublisherActorPID} ->
            {ok, PublisherActorPID};
        _ ->
            start(PublisherChannel, StreamName)
    end.

start(PublisherChannel, StreamName) ->
    gen_server:start(?MODULE, [PublisherChannel, StreamName], []).


register_viewer(PublisherActorPid, {EncoderPid, StreamID}) ->
    gen_server:cast(PublisherActorPid, {register_actor, EncoderPid, StreamID}).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([PublisherChannel, StreamName]) ->
    erlang:monitor(process, PublisherChannel),
    ok = rtmp_cluster:set_actor({?PUBLISHER_ACTOR_PREFIX, StreamName}, self()),
    {ok, #state{related_to_channel = PublisherChannel}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({register_actor, EncoderPid, StreamID}, #state{actors_list = ActorsList,
                                                           stream_info_amf_bin = StreamInfo,
                                                           key_frame = KeyFrame} = State) ->
    NewActorList = ActorsList ++ [{EncoderPid, StreamID}],
    NewState = State#state{actors_list = NewActorList},
    if(StreamInfo =/= undefined) ->
            ?LOG_DEBUG("send stream info: sid:~p", [StreamID]),
            rtmp_encode:send_message(EncoderPid, StreamID, {?RTMP_MSG_DATA_AMF0_BIN, StreamInfo}),
            rtmp_encode:send_message(EncoderPid, StreamID, {?RTMP_MSG_VIDEO, KeyFrame});
      true ->
            ok
    end,
    erlang:monitor(process, EncoderPid),
    ?LOG_INFO("register actor ~p, sid: ~p", [EncoderPid, StreamID]),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({data, Data, MsgCode}, #state{actors_list = ActorsList} = State) ->
    {ReformedMsgCode, NewState} = case MsgCode of
                                      ?RTMP_MSG_AUDIO ->
                                          {?RTMP_MSG_AUDIO, State};
                                      ?RTMP_MSG_VIDEO ->
                                          State1 = case Data of
                                                       <<23:8, _ELSE/binary>> ->
                                                           if erlang:byte_size(Data) < 36 ->
                                                                   State#state{key_frame = Data};
                                                              true ->
                                                                   State
                                                           end;

                                                       _ELSE ->
                                                           State
                                                   end,
                                          {?RTMP_MSG_VIDEO, State1};
                                      ?RTMP_MSG_DATA_AMF0 ->
                                          {?RTMP_MSG_DATA_AMF0_BIN,
                                           State#state{stream_info_amf_bin = Data}}
                                  end,
    Message = {ReformedMsgCode, Data},
    [rtmp_encode:send_message(Encoder, StreamID, Message) || {Encoder, StreamID} <- ActorsList],
    {noreply, NewState};

%% {ok, IoDevice} = file:open("/home/aram/test_rtmp.h264", [append]),
%% ok = file:write(IoDevice, Data),
%% file:close(IoDevice),
handle_info({'DOWN', _Ref, process, ChannelPid, _Reason},
            #state{related_to_channel = ChannelPid} = State) ->
    ?LOG_ERROR("close publisher connection -> (channel pid: ~p),", [ChannelPid]),
    {stop, normal, State};

handle_info({'DOWN', _Ref, process, EncoderPid, _Reason},
            #state{actors_list = ActorList} = State) ->
    NewActorList = proplists:delete(EncoderPid, ActorList),
    ?LOG_ERROR("close viewer connection -> (encoder pid: ~p), number of remaind viewer -> ~p", [EncoderPid, length(NewActorList)]),
    NewState = State#state{actors_list = NewActorList},
    {noreply, NewState};

handle_info(_info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
