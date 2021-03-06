%%%-------------------------------------------------------------------
%%% @author Benjamin Schubert, Basile Vu, Ioannis Noukakis and Sarra Berich
%%% @doc
%%% A module regrouping utility functions.
%%% @end
%%%-------------------------------------------------------------------
-module(sb_utils).

%% API
-export([flush/0]).


%% @doc flush the message queue.
flush() ->
  receive
    _ -> flush()
  after
    0 -> ok
  end.