%%%-------------------------------------------------------------------
%%% @author Benjamin Schubert, Basile Vu, Ioannis Noukakis and Sarra Berich
%%% @doc This is the module responsible of the sentiments analysis. It
%%% manages the storage as well as the sentiment regex evaluations.
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

%% @doc evaluates the current rule -> return a sentiment or notfound
%%
%% The first parameter is the message to analyse (string).
%%
%% The second parameter is the user how has issued this message (string).
analyze(Message, User) -> gen_server:call(?MODULE, {analyze, Message, User}).


%% @doc adds a new sentiment recognition rule at the beginning of the sentiments files.
%%
%% The first parameter is a regex (string).
%%
%% The second parameter is the sentiment (string).
add({Regex, Sentiment}) -> gen_server:call(?MODULE, {add, {Regex, Sentiment}}).


%% @doc deletes the sentiment rule at a current index.
%%
%% The first parameter is the the index where the sentiment and its rule will be deleted (int).
delete(Index) -> gen_server:call(?MODULE, {delete, Index}).


%% @doc return the actual rules for the currents sentiments.
dump() -> gen_server:call(?MODULE, dump).


%% @doc adds a new sentiment recognition rule at the given index.
%%
%% The first parameter is a regex (string).
%%
%% The second parameter is the sentiment (string).
%%
%% The third parameter is the the index where the sentiment and its rule will be inserted (int).
insert({Regex, Sentiment}, Index) -> gen_server:call(?MODULE, {insert, {Regex, Sentiment, Index}}).


%% @doc saves the current sentiment rule.
save() -> gen_server:call(?MODULE, save).


%% @doc move the current rule from OLD_INDEX to NEW_INDEX
%%
%% The first parameter is the old index (int).
%%
%% The second parameter is the new index (int).
move(OldIndex, NewIndex) -> gen_server:call(?MODULE, {move, {OldIndex, NewIndex}}).


%%=============================================================================
%% OTP API
%%=============================================================================

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%%=============================================================================
%% GENSERVER API
%%=============================================================================

%% @doc Standard OTP call.
init(_Settings) ->
  process_flag(trap_exit, true),
  {ok, Sentiments} = load_sentiments(),
  {ok, #state{sentiments = Sentiments}}.


%% @doc saves the current sentiment rule.
%%
%% The first parameter is the sentiment to be saved.
handle_call(save, _From, #state{sentiments = Sentiments} = State) ->
  dump_sentiments(Sentiments),
  {reply, ok, State};


%% @doc deletes the sentiment rule at a current index.
%%
%% The first parameter is the the index where the sentiment and its rule will be deleted (int).
handle_call({delete, Index}, _From, #state{sentiments = Sentiments} = State) ->
  case sb_list:remove_element(Index, Sentiments) of
    out_of_bound -> {reply, out_of_bound, State};
    {ok, NewSentiments, RemovedSentiment} -> {reply, {ok, RemovedSentiment}, State#state{sentiments = NewSentiments}}
  end;


%% @doc return the actual rules for the currents sentiments.
handle_call(dump, _From, #state{sentiments = Sentiments} = State) ->
  {reply, {ok, Sentiments}, State};


%% @doc adds a new sentiment recognition rule at the beginning of the sentiments files.
%%
%% The first parameter is a regex (string).
%%
%% The second parameter is the sentiment (string).
handle_call({add, {Regex, Sentiment}}, _From, #state{sentiments = Sentiments} = State) ->
  {reply, {ok, Sentiment}, State#state{sentiments = [{Regex, Sentiment} | Sentiments]}};


%% @doc adds a new sentiment recognition rule at the given index.
%%
%% The first parameter is a regex (string).
%%
%% The second parameter is the sentiment (string).
%%
%% The third parameter is the the index where the sentiment and its rule will be inserted (int).
handle_call({insert, {Regex, Sentiment, Index}}, _From, #state{sentiments = Sentiments} = State) ->
  case sb_list:insert_element({Regex, Sentiment}, Index, Sentiments) of
    {out_of_bound} -> {reply, out_of_bound, State};
    {ok, List} -> {reply, ok, State#state{sentiments = List}}
  end;


%% @doc move the current rule from OLD_INDEX to NEW_INDEX
%%
%% The first parameter is the old index (int).
%%
%% The second parameter is the new index (int).
handle_call({move, {OldIndex, NewIndex}}, _From, #state{sentiments = Sentiments} = State) ->
  case move_sentiment(OldIndex, NewIndex, Sentiments) of
    {out_of_bound, Index} -> {reply, {out_of_bound, Index}, State};
    {ok, NewSentiments} -> {reply, ok, State#state{sentiments = NewSentiments}}
  end;


%% @doc evaluates the current rule -> return a sentiment or notfound
%%
%% The first parameter is the message to analyse (string).
%%
%% The second parameter is the user how has issued this message (string).
handle_call({analyze, Message, User}, _From, #state{sentiments = Sentiments} = State) ->
  case extract_sentiment(Message, Sentiments) of
    notfound -> {reply, notfound, State};
    {match, Sentiment} -> {reply, {ok, Sentiment}, State}
  end.


%% @doc default behavior
handle_cast(_Request, State) -> {noreply, State}.


%% @doc default behavior
handle_info(_Message, State) -> {noreply, State}.


%% @doc termination handler. Prints an error message, clean the mailbox and let the program exit.
%%
%% The first parameter is the reason of the shutdown (atom)
terminate(_Reason, _State) ->
  lager:error(<<"[BIBI-BOT][Sentiment analysis] Shutting down... Reason is: ~p">>, [_Reason]),
  sb_utils:flush(), %% empty mailbox in case there is still a message upon shutdown
  ok.


%% @doc default behavior
code_change(_OldVersion, State, _Extra) -> {ok, State}.


%%=============================================================================
%% INTERNAL HELPERS
%%=============================================================================

%% @doc loads the sentiments from the configuration file into the server's list.
load_sentiments() ->
  Result = file:consult(sb_config:get_sentiment_file()),

  case Result of
    {error, enoent} -> {ok, []};
    {ok, Sentiments} -> {ok, Sentiments}
  end.


%% @doc prints all the server's list sentiments.
%%
%% The first parameter is the list to be printed (List[Sentiment])
dump_sentiments(Sentiments) -> file:write_file(
  sb_config:get_sentiment_file(),
  lists:map(fun(Term) -> io_lib:format("~tp.~n", [Term]) end, Sentiments)
).


%% @doc evaluates a regex and extract a sentiment if one match the regex. Notfound otherwise.
%%
%% The first parameter is the message to process (string)
extract_sentiment(_Message, []) -> notfound;

extract_sentiment(Message, [{Regexp, Sentiment} | Tail]) ->
  case re:run(Message, Regexp) of
    {match, _} -> {match, Sentiment};
    nomatch -> extract_sentiment(Message, Tail)
  end.


%% @doc move the current rule from OLD_INDEX to NEW_INDEX
%%
%% The first parameter is the old index (int).
%%
%% The second parameter is the new index (int).
%%
%% The third parameter is the list of sentiment (List[Sentiment])
move_sentiment(OldIndex, NewIndex, Sentiments) ->
  case sb_list:remove_element(OldIndex, Sentiments) of
    out_of_bound -> {out_of_bound, OldIndex};
    {ok, List, Element} ->
      case sb_list:insert_element(Element, NewIndex, List) of
        out_of_bound -> {out_of_bound, NewIndex};
        Else -> Else
      end
  end.