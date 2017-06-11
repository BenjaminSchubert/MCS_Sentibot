%%%-------------------------------------------------------------------
%%% @author Benjamin Schubert, Basile Vu, Ioannis Noukakis and Sarra Berich
%%% @doc
%%% FIXME
%%% @end
%%%-------------------------------------------------------------------
-module(sb_bot_intelligence).
-behavior(gen_server).


%% public api
-export([handle/1]).

%% otp api
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-record(state, {
  botId,
  botIdPattern,
  botName
}).


%%=============================================================================
%% PUBLIC API
%%=============================================================================

% FIXME: document
handle(Message) ->
  gen_server:cast(?MODULE, {message, Message}).


%%=============================================================================
%% OTP API
%%=============================================================================

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%%=============================================================================
%% GENSERVER API
%%=============================================================================

init(_Settings) ->
  {ok, 200, _Headers, ResponseList} = slacker_auth:test(sb_config:get_slack_token()),
  Response = maps:from_list(ResponseList),
  {ok, BotId} = maps:find(<<"user_id">>, Response),
  {ok, BotName} = maps:find(<<"user">>, Response),
  State = #state{botIdPattern = binary:compile_pattern(BotId), botId = BotId, botName = BotName},

  {ok, State}.


handle_cast({message, Message}, #state{botIdPattern = Pattern} = State) ->
  Bot = maps:find(bot_id, Message),

  case Bot of
    {ok, _} -> ok;
    error ->
      {ok, Text} = maps:find(text, Message),
      {ok, Channel} = maps:find(channel, Message),
      {ok, User} = maps:find(user, Message),

      Command = binary:match(Text, Pattern),
      case Command of
        nomatch -> handle_message({Text, Channel, User}, State);
        _Else -> handle_command({Text, Channel, User}, State)
      end
  end,

  {noreply, State}.


handle_call(_Request, _From, State) -> {noreply, State}.
handle_info(_Message, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.


%%=============================================================================
%% INTERNAL API
%%=============================================================================

%%-----------------------------------------------------------------------------
%% Types of available messages to treat
%%-----------------------------------------------------------------------------
handle_command({Text, Channel, User}, #state{botId = BotID, botName = BotName}) ->
  {ok, Regex} = re:compile(<<"^<@", BotID/binary, "> (?<Command>.+)">>),

  case re:run(Text, Regex, [{capture, all_names, binary}]) of
    {match, [Command]} ->
      case split_on_space(Command) of
        error -> send_message(Channel, <<"Sorry <@", User/binary, ">, I was unable to parse your command.">>);
        List -> handle_command(List, Channel, User, BotName)
      end;
    _Else -> send_unknown_command(Channel, User, BotName)
  end.


handle_message({Message, Channel, User}, _State) ->
  Result = sb_sentiment_analysis:analyze(Message, User),
  case Result of
    {ok, Sentiment} -> send_message(Channel, Sentiment);
    notfound -> ok
  end.


%%-----------------------------------------------------------------------------
%% COMMAND HANDLERS
%%-----------------------------------------------------------------------------

handle_command([<<"help">>], Channel, _User, BotName) -> send_help(Channel, BotName);

handle_command([<<"add">>, Tail, Sentiment], Channel, _User, _BotName) ->
  {ok, Sentiment} = sb_sentiment_analysis:add({Tail, Sentiment}),
  send_message(Channel, <<"New sentiment recognition added for ", Sentiment/binary>>);

handle_command([<<"delete">>, IndexString], Channel, User, _BotName) ->
  try binary_to_integer(IndexString) of
    Index ->
      case sb_sentiment_analysis:delete(Index) of
        {ok, {R, S}} -> send_message(Channel, <<R/binary, " -> ", S/binary, "has been deleted">>);
        out_of_bound -> send_message(Channel, <<(48 + Index), " is out of bound.">>)
      end
  catch
    error:badarg -> send_message(Channel, <<"<@", User/binary, ">, This is not an integer, I can't delete at this index !">>)
  end;

handle_command([<<"dump">>], Channel, _User, _BotName) ->
  {ok, Rules} = sb_sentiment_analysis:dump(),
  send_message(Channel, sb_utils:mapWithIndex(
    fun({R, Sentiment}, Index) -> <<(48 + Index), ". `", R/binary, "` -> ", Sentiment/binary, "\n">> end,
    Rules
  ));

handle_command([<<"save">>], Channel, _User, _BotName) ->
  ok = sb_sentiment_analysis:save(),
  send_message(Channel, <<"Rules successfully saved">>);

handle_command(_List, Channel, User, BotName) ->
  send_unknown_command(Channel, User, BotName).

%%-----------------------------------------------------------------------------
%% RESPONSES to Slack
%%-----------------------------------------------------------------------------
send_unknown_command(Channel, User, BotName) -> send_message(
  Channel,
  [
    <<"Sorry <@">>,
    User,
    <<">, I could not understand. Correct format is '@">>,
    BotName,
    <<" <command>'.\nPlease use '@">>,
    BotName,
    <<" help' for more information.">>
  ]
).

send_help(Channel, BotName) -> send_message(
  Channel,
  <<
    "```@", BotName/binary, " COMMAND [PARAMETERS]...",
    "\n\nAvailable Commands:",
    "\n\thelp                     Display this help message",
    "\n\tdelete INDEX             Delete the rule at the given index",
    "\n\tdump                     Dump the current rules",
    "\n\tinsert REGEX SENTIMENT   Add a new sentiment recognition at the end of the list",
    "\n\tsave                     Save the current rules",
    "```"
  >>
).


%%-----------------------------------------------------------------------------
%% HELPERS
%%-----------------------------------------------------------------------------
help_fm(Name, Msg) -> list_to_binary([<<"\n\t">>, Name, <<"\t">>, Msg]).

send_message(Channel, [_Head | _Tail] = List) ->
  send_message(Channel, list_to_binary(List));

send_message(Channel, Message) ->
  slacker_chat:post_message(sb_config:get_slack_token(), Channel, Message, [{as_user, true}]).


split_on_space(String) ->
  Split = re:split(String, <<" ">>),
  join_on_quotation_mark(Split).


join_on_quotation_mark(L) -> join_on_quotation_mark(L, []).

join_on_quotation_mark([], Acc) -> lists:reverse(Acc);

join_on_quotation_mark([<<"\"", Head/binary>> | Tail], Acc) ->
  case binary:last(Head) of
    $" -> join_on_quotation_mark(Tail, [binary:part(Head, 0, byte_size(Head) - 1) | Acc]);
    _Else ->  consume_until_quotation_mark(Tail, Acc, [Head])
  end;

join_on_quotation_mark([Head | Tail], Acc) -> join_on_quotation_mark(Tail, [Head | Acc]).

consume_until_quotation_mark([], _Acc, _Value) -> error;

consume_until_quotation_mark([Head | Tail], Acc, Value) ->
  case binary:last(Head) of
    $" -> join_on_quotation_mark(
      Tail,
      [list_to_binary(lists:reverse([binary:part(Head, 0, byte_size(Head) - 1), <<" ">> | Value])) | Acc]
    );
    _Else -> consume_until_quotation_mark(Tail, Acc, [Head, <<" ">> | Value])
  end.
