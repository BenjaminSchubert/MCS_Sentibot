%%%-------------------------------------------------------------------
%%% @author Benjamin Schubert, Basile Vu, Ioannis Noukakis and Sarra Berich
%%% @doc
%%% Helper to access sentibot configuration
%%% @end
%%%-------------------------------------------------------------------
-module(sb_config).

-define(APP, sentibot).


%% API
-export([get_slack_token/0]).


get_slack_token() -> get_value(slack_token).


%% internal helpers
get_value(Atom) ->
  {ok, Value} = application:get_env(?APP, Atom),
  Value.
