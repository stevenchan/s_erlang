%%-------------------------------------------------------------------
%% @author Steven Chan <contact@stevenchan.hk>
%% @doc 
%% @end
%%-------------------------------------------------------------------
-module(s_erlang).

%% API
-export([uuid/0, start_apps/1]).

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% @spec uuid() -> binary()
%% @doc UUID based on random numbers. Side effect: starting crypto
%% @end
%%--------------------------------------------------------------------
uuid() ->
    ok = start_apps([crypto]),
    list_to_binary(to_hex(crypto:rand_bytes(16))).
    
to_hex([]) ->
    [];
to_hex(Bin) when is_binary(Bin) ->
    to_hex(binary_to_list(Bin));
to_hex([H|T]) ->
    [to_digit(H div 16), to_digit(H rem 16) | to_hex(T)].

to_digit(N) when N < 10 -> $0 + N;
to_digit(N)             -> $a + N-10.

%%--------------------------------------------------------------------
%% @spec start_apps([Application::atom()]) -> ok | {error, Reason}
%% @doc 
%% @end
%%--------------------------------------------------------------------
start_apps([]) ->
    ok;
start_apps([App|Rest]) ->
    case application:start(App) of
        ok ->
           start_apps(Rest);
        {error, {already_started, App}} ->
           start_apps(Rest);
        {error, _Reason} ->
           {error, {unable_to_start, App}}
    end.

%%====================================================================
%% Internal functions
%%====================================================================
