%%%-------------------------------------------------------------------
%%% @author Benjamin Schubert, Basile Vu, Ioannis Noukakis and Sarra Berich
%%% @doc
%%% FIXME
%%% @end
%%%-------------------------------------------------------------------
-module(sb_list).

-export([insert_element/3, remove_element/2]).


remove_element(Index, List) ->
  case Index of
    _ when Index < 0 -> out_of_bound;
    _ -> remove_element(Index, List, [])
  end.

remove_element(_Index, [], _Acc) -> out_of_bound;

remove_element(0, [Head | Tail], Acc) -> {ok, lists:reverse(Acc) ++ Tail, Head};

remove_element(Index, [Head | Tail], Acc) -> remove_element(Index - 1, Tail, [Head | Acc]).


insert_element(Element, Index, List) ->
  case Index of
    _ when Index < 0 -> out_of_bound;
    _ -> insert_element(Element, Index, List, [])
  end.

insert_element(Element, 0, List, Acc) -> {ok, lists:reverse(Acc) ++ [Element | List]};

insert_element(_Element, _Index, [], _Acc) -> out_of_bound;

insert_element(Element, Index, [Head | Tail], Acc) -> insert_element(Element, Index - 1, Tail, [Head | Acc]).