%%%-------------------------------------------------------------------
%%% @author Benjamin Schubert, Basile Vu, Ioannis Noukakis and Sarra Berich
%%% @doc
%%% A module regrouping utility functions.
%%% @end
%%%-------------------------------------------------------------------
-module(sb_utils).

%% API
-export([mapWithIndex/2, flush/0]).

%% @spec (Function, List) -> List
%%
%% @doc Applies a function on each element of the given list, along with its the index.
%%
%% The first parameter is a function that takes two parameters, a value and its index.
%% The second parameter is the list containing the values.
mapWithIndex(Function, List) -> mapWithIndex(Function, List, [], 0).

mapWithIndex(_Function, [], Acc, _Index) -> lists:reverse(Acc);

mapWithIndex(Function, [Head | Tail], Acc, Index) ->
  mapWithIndex(Function, Tail, [Function(Head, Index) | Acc], Index + 1).

flush() ->
  receive
    _ -> flush()
  after
    0 -> ok
  end.