%%%-------------------------------------------------------------------
%%% @author Benjamin Schubert, Basile Vu, Ioannis Noukakis and Sarra Berich
%%% @doc
%%% FIXME
%%% @end
%%%-------------------------------------------------------------------
-module(sb_sentiment_analysis).
-behavior(gen_server).


%% public api
-export([analyze/2]).

%% otp api
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%=============================================================================
%% Public API
%%=============================================================================
analyze(Message, User) ->
  gen_server:call(?MODULE, {Message, User}).


%%=============================================================================
%% OTP API
%%=============================================================================

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%%=============================================================================
%% GENSERVER API
%%=============================================================================

init(_Settings) ->
  {ok, []}.


handle_call({Message, User}, _From, State) ->
  {reply, {ok, list_to_binary([<<"<@">>, User, <<"> said ">>, Message])}, State}.


handle_cast(_Request, State) -> {noreply, State}.
handle_info(_Message, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
