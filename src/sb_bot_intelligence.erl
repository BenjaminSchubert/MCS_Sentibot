%%%-------------------------------------------------------------------
%%% @author Benjamin Schubert, Basile Vu, Ioannis Noukakis and Sarra Berich
%%% @doc
%%% FIXME
%%% @end
%%%-------------------------------------------------------------------
-module(sb_bot_intelligence).
-author("tellendil").

%% API
-export([handle_message/2]).


% FIXME: document
handle_message(Message, _AuthToken) ->
  lager:info("Bot needs to handle ~p", [Message]).