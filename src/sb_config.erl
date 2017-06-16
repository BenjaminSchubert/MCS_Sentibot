%%%-------------------------------------------------------------------
%%% @author Benjamin Schubert, Basile Vu, Ioannis Noukakis and Sarra Berich
%%% @doc
%%% Helper to access sentibot configuration
%%% @end
%%%-------------------------------------------------------------------
-module(sb_config).

-define(APP, sentibot).


%% API
-export([get_slack_token/0, get_sentiment_file/0]).


%% @doc get the slack authentication token.
get_slack_token() -> get_value(slack_token).


%% @doc get the path to the sentiment file.
get_sentiment_file() -> get_value(sentiment_file).


%% internal helpers
get_value(Atom) ->
  Result = application:get_env(?APP, Atom),

  case Result of
    {ok, Value} -> Value;
    _Else -> lager:error("No configuration information found for \"~p\"", [Atom])
  end.
