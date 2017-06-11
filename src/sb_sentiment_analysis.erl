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
-export([analyze/2, add/1, delete/1, dump/0, insert/2, move/2, save/0]).

%% otp api
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%=============================================================================
%% Public API
%%=============================================================================
analyze(Message, User) -> gen_server:call(?MODULE, {analyze, Message, User}).

add({Regex, Sentiment}) -> gen_server:call(?MODULE, {add, {Regex, Sentiment}}).

delete(Index) -> gen_server:call(?MODULE, {delete, Index}).

dump() -> gen_server:call(?MODULE, dump).

insert({Regex, Sentiment}, Index) -> gen_server:call(?MODULE, {insert, {Regex, Sentiment, Index}}).

save() -> gen_server:call(?MODULE, save).

move(OldIndex, NewIndex) -> gen_server:call(?MODULE, {move, {OldIndex, NewIndex}}).


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


handle_call(save, _From, #state{sentiments = Sentiments} = State) ->
  dump_sentiments(Sentiments),
  {reply, ok, State};

handle_call({delete, Index}, _From, #state{sentiments = Sentiments} = State) ->
  case sb_list:remove_element(Index, Sentiments) of
    out_of_bound -> {reply, out_of_bound, State};
    {ok, NewSentiments, RemovedSentiment} -> {reply, {ok, RemovedSentiment}, State#state{sentiments = NewSentiments}}
  end;

handle_call(dump, _From, #state{sentiments = Sentiments} = State) ->
  {reply, {ok, Sentiments}, State};

handle_call({add, {Regex, Sentiment}}, _From, #state{sentiments = Sentiments} = State) ->
  {reply, {ok, Sentiment}, State#state{sentiments = [{Regex, Sentiment} | Sentiments]}};

handle_call({insert, {Regex, Sentiment, Index}}, _From, #state{sentiments = Sentiments} = State) ->
  case sb_list:insert_element({Regex, Sentiment}, Index, Sentiments) of
    {out_of_bound} -> {reply, out_of_bound, State};
    {ok, List} -> {reply, ok, State#state{sentiments = List}}
  end;

handle_call({move, {OldIndex, NewIndex}}, _From, #state{sentiments = Sentiments} = State) ->
  case move_sentiment(OldIndex, NewIndex, Sentiments) of
    {out_of_bound, Index} -> {reply, {out_of_bound, Index}, State};
    {ok, NewSentiments} -> {reply, ok, State#state{sentiments = NewSentiments}}
  end;

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
    {error, enoent} -> {ok, []};
    {ok, Sentiments} -> {ok, Sentiments}
  end.


dump_sentiments(Sentiments) -> file:write_file(
  sb_config:get_sentiment_file(),
  lists:map(fun(Term) -> io_lib:format("~tp.~n", [Term]) end, Sentiments)
).


extract_sentiment(_Message, []) -> notfound;

extract_sentiment(Message, [{Regexp, Sentiment} | Tail]) ->
  case re:run(Message, Regexp) of
    {match, _} -> {match, Sentiment};
    nomatch -> extract_sentiment(Message, Tail)
  end.


move_sentiment(OldIndex, NewIndex, Sentiments) ->
  case sb_list:remove_element(OldIndex, Sentiments) of
    out_of_bound -> {out_of_bound, OldIndex};
    {ok, List, Element} ->
      case sb_list:insert_element(Element, NewIndex, List) of
        out_of_bound -> {out_of_bound, NewIndex};
        Else -> Else
      end
  end.