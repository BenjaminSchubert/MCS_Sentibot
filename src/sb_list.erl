%%%-------------------------------------------------------------------
%%% @author Benjamin Schubert, Basile Vu, Ioannis Noukakis and Sarra Berich
%%% @doc This module offers various helpers to handle lists
%%%-------------------------------------------------------------------
-module(sb_list).

-export([insert_element/3, remove_element/2, mapWithIndex/2]).


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


%% @spec (Index, List) -> List
%%
%% @doc remove the element at Index from the given list
remove_element(Index, List) ->
  case Index of
    _ when Index < 0 -> out_of_bound;
    _ -> remove_element(Index, List, [])
  end.

remove_element(_Index, [], _Acc) -> out_of_bound;

remove_element(0, [Head | Tail], Acc) -> {ok, lists:reverse(Acc) ++ Tail, Head};

remove_element(Index, [Head | Tail], Acc) -> remove_element(Index - 1, Tail, [Head | Acc]).


%% @spec (Element, Index, List)
%%
%% @doc Add the given Element at the given Index in the List.
insert_element(Element, Index, List) ->
  case Index of
    _ when Index < 0 -> out_of_bound;
    _ -> insert_element(Element, Index, List, [])
  end.

insert_element(Element, 0, List, Acc) -> {ok, lists:reverse(Acc) ++ [Element | List]};

insert_element(_Element, _Index, [], _Acc) -> out_of_bound;

insert_element(Element, Index, [Head | Tail], Acc) -> insert_element(Element, Index - 1, Tail, [Head | Acc]).
