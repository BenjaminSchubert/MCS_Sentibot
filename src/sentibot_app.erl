%%%-------------------------------------------------------------------
%%% @author Benjamin Schubert, Basile Vu, Ioannis Noukakis and Sarra Berich
%%% @doc
%%% Sentibot Public API
%%% @end
%%%-------------------------------------------------------------------

-module(sentibot_app).

-define(SUPERVISOR, sentibot_sup).
-export([start/2, stop/1]).


start(_Type, _StartArgs) ->
  io:format("~p~n", ["TEST2"]),
  io:format("~p~n", [self()]),
  lager:start(),
  {ok, Sup} = sentibot_sup:start_link(),
  {ok, Sup}.


stop(_State) ->
  lager:stop(),
  exit(whereis(?SUPERVISOR), shutdown).