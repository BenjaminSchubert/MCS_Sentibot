%%%-------------------------------------------------------------------
%% @doc sentibot public API
%% @end
%%%-------------------------------------------------------------------

-module(sentibot_app).

-define(SUPERVISOR, sentibot_sup).
-export([start/2, stop/1]).


start(_Type, _StartArgs) ->
  lager:start(),
  {ok, Sup} = sentibot_sup:start_link(),
  {ok, Sup}.


stop(_State) ->
  lager:stop(),
  exit(whereis(?SUPERVISOR), shutdown).