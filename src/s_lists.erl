%%-------------------------------------------------------------------
%% @author Steven Chan <contact@stevenchan.hk>
%% @doc 
%% @end
%%-------------------------------------------------------------------
-module(s_lists).

%% API
-export([random/1,
         shuffle/1]).

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% @spec random(List) -> any()
%% @doc Return a random element
%% @end
%%--------------------------------------------------------------------
random(List) ->
    lists:nth(random:uniform(length(List)), List).

%%--------------------------------------------------------------------
%% @spec shuffle(list()) -> list()
%% @doc
%% @end
%%--------------------------------------------------------------------
shuffle(List) ->
%% Determine the log n portion then randomize the list.
   randomize(round(math:log(length(List)) + 0.5), List).

randomize(1, List) ->
   randomize(List);
randomize(T, List) ->
   lists:foldl(fun(_E, Acc) ->
                  randomize(Acc)
               end, randomize(List), lists:seq(1, (T - 1))).

randomize(List) ->
   D = lists:map(fun(A) ->
                    {random:uniform(), A}
             end, List),
   {_, D1} = lists:unzip(lists:keysort(1, D)), 
   D1.

%%====================================================================
%% Internal functions
%%====================================================================
