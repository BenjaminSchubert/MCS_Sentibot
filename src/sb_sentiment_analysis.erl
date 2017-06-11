%%%-------------------------------------------------------------------
%%% @author tellendil
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Jun 2017 15:22
%%%-------------------------------------------------------------------
-module(sb_sentiment_analysis).
-author("tellendil").

%% API
-export([analyze/2]).


analyze(Message, User) ->
  {ok, list_to_binary([<<"<@">>, User, <<"> said ">>, Message])}.