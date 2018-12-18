%%====================================================================
%% Description: RTMP application event manager
%%====================================================================

-module(rtmp_event).

-author("Artem Ekimov <ekimov-artem@ya.ru>").

%% API functions
-compile(export_all).

-include("rtmp.hrl").

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
	gen_event:start_link({local, ?MODULE}).

add_handler(Handler, Args) ->
	gen_event:add_handler(?MODULE, Handler, Args).

add_sup_handler(Handler, Args) ->
	gen_event:add_sup_handler(?MODULE, Handler, Args).

notify({rtmp_event, Channel, TrID, {new_connection, ConnectInfo}}) ->
	?LOG_DEBUG("connect event: ~p", [ConnectInfo]),
	rtmp_app:accept_connection(Channel, TrID);

notify({rtmp_event, Channel, {start_publish, StreamRef, StreamName, Type}}) ->
	?LOG_DEBUG("publish event: Ref => ~p | StreamName => ~p | Type => ~p", [StreamRef, StreamName, Type]),
    {ok , Pid} = rtmp_publish:get_pid(publisher, Channel, StreamName),
	rtmp_app:accept_publish(Channel, StreamRef, Pid);

notify({rtmp_event, Channel, {start_play, StreamRef, StreamName}}) ->
	?LOG_DEBUG("play event: Ref => ~p | StreamName => ~p ", [StreamRef, StreamName]),
    case rtmp_publish:get_pid(viewer, StreamName) of
        {ok, _Pid} ->
            rtmp_app:accept_play(Channel, StreamRef);

        this_stream_is_not_published_right_now ->
            ?LOG_ERROR("this stream name not avalable to play => ~p", [StreamName]),
            ok
    end;

notify({rtmp_event, _Channel, {register_viewer, StreamName, SID, EncoderPid}}) ->
	?LOG_DEBUG("register viewer event: EncoderPid => ~p | StreamName => ~p | SID => ~p", [EncoderPid, StreamName, SID]),
    case rtmp_publish:get_pid(viewer, StreamName) of
        {ok, Pid} ->
            rtmp_publish:register_viewer(Pid, {EncoderPid, SID});

        this_stream_is_not_published_right_now ->
            ?LOG_ERROR("this stream name not avalable to play => ~p", [StreamName]),
            ok
    end;

notify(Event) ->
	?LOG_DEBUG("unhandled event: ~p", [Event]),
	ok.

%% notify(Event) ->
%%  	gen_event:notify(?MODULE, Event).

sync_notify(Event) ->
	gen_event:sync_notify(?MODULE, Event).

call(Handler, Request) ->
	gen_event:call(?MODULE, Handler, Request).

call(Handler, Request, Timeout) ->
	gen_event:call(?MODULE, Handler, Request, Timeout).

handle_event(Event, State) ->
	?LOG_DEBUG("unhandled event: ~p", [Event]),
	{ok, State}.

delete_handler(Handler, Args) ->
	gen_event:delete_handler(?MODULE, Handler, Args).

swap_handler({Handler1, Args1}, {Handler2, Args2}) ->
	gen_event:swap_handler(?MODULE, {Handler1, Args1}, {Handler2, Args2}).

swap_sup_handler({Handler1, Args1}, {Handler2, Args2}) ->
	gen_event:swap_sup_handler(?MODULE, {Handler1, Args1}, {Handler2, Args2}).

which_handlers() ->
	gen_event:which_handlers(?MODULE).

stop() ->
	gen_event:stop(?MODULE).
