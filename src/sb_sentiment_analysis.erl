%%%-------------------------------------------------------------------
%%% @author Benjamin Schubert, Basile Vu, Ioannis Noukakis and Sarra Berich
%%% @doc
%%% FIXME
%%% @end
%%%-------------------------------------------------------------------
-module(sb_sentiment_analysis).
-behavior(gen_server).


-record(state, {
  sentiments,
  current
}).

%% public api
-export([analyze/2]).

%% otp api
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, add/1]).

%%=============================================================================
%% Public API
%%=============================================================================
analyze(Message, User) ->
  gen_server:call(?MODULE, {analyze, Message, User}).


add(Message) ->
  gen_server:call(?MODULE, {add, Message}).


%%=============================================================================
%% OTP API
%%=============================================================================

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%%=============================================================================
%% GENSERVER API
%%=============================================================================

init(_Settings) ->
  {ok, Sentiments} = load_sentiments(),
  {ok, #state{sentiments = Sentiments}}.


handle_call({add, {Regex, Sentiment}}, _From, #state{sentiments = Sentiments} = State) ->
  {reply, {ok, Sentiment}, State#state{sentiments = [{Regex, Sentiment} | Sentiments]}};


handle_call({analyze, Message, User}, _From, #state{sentiments = Sentiments} = State) ->
  case extract_sentiment(Message, Sentiments) of
    notfound -> {reply, notfound, State};
    {match, Sentiment} -> {reply, {ok, [<<"<@">>, User, <<"> is ">>, Sentiment, <<".">>]}, State}
  end.


handle_cast(_Request, State) -> {noreply, State}.
handle_info(_Message, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.


%%=============================================================================
%% INTERNAL HELPERS
%%=============================================================================

load_sentiments() ->
  Result = file:consult(sb_config:get_sentiment_file()),

  case Result of
    {error, enoent} -> {ok, []}
  end.


extract_sentiment(_Message, []) ->
  notfound;

extract_sentiment(Message, [{Regexp, Sentiment} | Tail]) ->
  case re:run(Message, Regexp) of
    {match, _} -> {match, Sentiment};
    nomatch -> extract_sentiment(Message, Tail)
  end.
