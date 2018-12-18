-module(rtmp_cluster).

-export([set_actor/2,
         unset_actor/1,
         get_actor_pid/1,
         get_actor_name/1,
         get_actor_counts/1]).

-type actor_name() :: any().
-type actor_pid() :: pid().

-define(BACKEND, syn).

-spec set_actor(actor_name(), actor_pid()) -> ok |
                                              {taken_pid, actor_name()} |
                                              {taken_name, actor_pid()} |
                                              {error, not_consistent}.

set_actor(Name, PID) ->
    case ?BACKEND:register(Name, PID) of
        ok ->
            ok;

        {error, taken} ->
            case get_actor_pid(Name) of
                {ok, TakenPID} ->
                    {take_pid, TakenPID};

                _ ->
                    {error, not_consistent}
            end;

        {error, pid_already_registered} ->
            case get_actor_name(PID) of
                {ok, TakenName} ->
                    {taken_name, TakenName};

                _ ->
                    {error, not_consistent}
            end
    end.

-spec unset_actor(actor_name()) -> ok |
                                   {error, not_found}.

unset_actor(Name) ->
    case ?BACKEND:unregister(Name) of
        ok ->
            ok;

        {error, undefined} ->
            {error, not_found}
    end.

-spec get_actor_pid(actor_name()) ->
                           {ok, actor_pid()} |
                           {error, not_found}.

get_actor_pid(Name) ->
    case ?BACKEND:find_by_key(Name) of
        undefined ->
            {error, not_found};

        PID ->
            {ok, PID}
    end.

-spec get_actor_name(actor_pid()) -> {ok, actor_name()} |
                                     {error, not_found}.

get_actor_name(PID) ->
    case ?BACKEND:find_by_pid(PID) of
        undefined ->
            {error, not_found};

        Name ->
            {ok, Name}
    end.

-spec get_actor_counts(atom()) -> non_neg_integer().
get_actor_counts(Node) ->
    ?BACKEND:registry_count(Node).
