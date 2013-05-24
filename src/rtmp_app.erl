%%%---------------------------------------------------------------------------------------------------------------------------------------------
%%% File        : rtmp_app.erl
%%% Author      : Artem A. Ekimov <ekimov-artem@ya.ru>
%%% Description : RTMP application API and callbacks module
%%% Created     : 28.04.2012
%%%---------------------------------------------------------------------------------------------------------------------------------------------

-module(rtmp_app).
-bahaviour(application).

-include("rtmp.hrl").

%% API functions

%% application callbacks

-export([start/2, stop/1]).

%%==============================================================================================================================================
%% API functions
%%==============================================================================================================================================

%%==============================================================================================================================================
%% application callbacks
%%==============================================================================================================================================

start(_Type, _Args) ->
	?LOG(?MODULE, self(), "start", []),
%	Res1 = register:start(),
%	Res2 = register:create(?REGISTER),
	gproc:start_link(),
%	?LOG(?MODULE, self(), "register: ~p, ~p", [Res1, Res2]),
	rtmp_sup:start().

stop(_State) ->
	ok.
	
%%==============================================================================================================================================
%% Internal functions
%%==============================================================================================================================================

%%%---------------------------------------------------------------------------------------------------------------------------------------------
%%% End of file : rtmp_app.erl
%%%---------------------------------------------------------------------------------------------------------------------------------------------
