%%%-------------------------------------------------------------------
%%% @author Benjamin Schubert, Basile Vu, Ioannis Noukakis and Sarra Berich
%%% @doc
%%% Sentibot Public API
%%% @end
%%%-------------------------------------------------------------------

-module(sentibot_app).

-define(SUPERVISOR, sentibot_sup).
-export([start/2, stop/1]).


%% @doc starts this application
%%
%% The first parameter is the type of the application.
%%
%% The second parameter is the arguments of the application (string).
start(_Type, _StartArgs) ->
  lager:start(),
  {ok, Sup} = sentibot_sup:start_link(),
  {ok, Sup}.

%% @doc stops this application
%%
%% The first parameter is the state of the application.
stop(_State) ->
  lager:stop(),
  exit(whereis(?SUPERVISOR), shutdown).