%%====================================================================
%% Description: RTMP application supervisor for rtmp_decode
%%====================================================================

-module(rtmp_decode_sup).

-author("Artem Ekimov <ekimov-artem@ya.ru>").

-bahaviour(supervisor).

-include("rtmp.hrl").

%% API functions
-export([start/0, start_link/0, start_decode/1]).

%% supervisor callbacks
-export([init/1]).

%%====================================================================
%% API functions
%%====================================================================

start() ->
	start_link().

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_decode(Args) ->
	supervisor:start_child(?MODULE, Args).

%%====================================================================
%% supervisor callbacks
%%====================================================================

init([]) ->
	{ok, {{simple_one_for_one, 1, 10}, [
                                        {undefined, {rtmp_decode, start_link, []}, temporary, 1000, worker, [rtmp_decode]}
                                       ]}}.
