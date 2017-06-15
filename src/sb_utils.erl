%%%-------------------------------------------------------------------
%%% @author Benjamin Schubert, Basile Vu, Ioannis Noukakis and Sarra Berich
%%% @doc
%%% FIXME
%%% @end
%%%-------------------------------------------------------------------
-module(sb_utils).

%% API
-export([mapWithIndex/2, flush/0]).


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