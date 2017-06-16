%%%-------------------------------------------------------------------
%%% @author Benjamin Schubert, Basile Vu, Ioannis Noukakis and Sarra Berich
%%% @doc
%%% Command handling and reaction to user messages. This is where the behavior the bot is defined.
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
  botName,
  stateOfMind
}).


%%=============================================================================
%% PUBLIC API
%%=============================================================================

%% @doc handles a message. If the message is for the bot it will parse it and do the action that is mapped with such message
%% If not the sentimental treatment is applied.
%%
%% The parameter is a message (Html Body).
handle(Body) ->
  Message = jsx:decode(Body, [return_maps, {labels, atom}]),
  {ok, Type} = maps:find(type, Message),
  case Type of
    <<"message">> -> gen_server:cast(?MODULE, {message, Message});
    _Else -> do_nothing
  end.


%%=============================================================================
%% OTP API
%%=============================================================================

%% @doc default behavior
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%%=============================================================================
%% GENSERVER API
%%=============================================================================

%% @doc default behavior
init(_Settings) ->
  process_flag(trap_exit, true),
  {ok, 200, _Headers, ResponseList} = slacker_auth:test(sb_config:get_slack_token()),
  Response = maps:from_list(ResponseList),
  {ok, BotId} = maps:find(<<"user_id">>, Response),
  {ok, BotName} = maps:find(<<"user">>, Response),
  State = #state{botIdPattern = binary:compile_pattern(BotId), botId = BotId, botName = BotName, stateOfMind = maps:new()},

  {ok, State}.


%% @doc handles a message. If the message is for the bot it will parse it and do the action that is mapped with such message
%% If not the sentimental treatment is applied.
%%
%% The first parameter is a message (string).
handle_cast({message, Message}, #state{botIdPattern = Pattern} = State) ->
  Bot = maps:find(bot_id, Message),

  case Bot of
    {ok, _} -> {noreply, State};
    error ->
      {ok, Text} = maps:find(text, Message),
      {ok, Channel} = maps:find(channel, Message),
      {ok, User} = maps:find(user, Message),

      Command = binary:match(Text, Pattern),
      case Command of
        nomatch ->
          {noreply, handle_message({Text, Channel, User}, State)};
        _Else ->
          handle_command({Text, Channel, User}, State),
          {noreply, State}
      end
  end.


%% @doc default behavior
handle_call(_Request, _From, State) -> {noreply, State}.


%% @doc default behavior
handle_info(_Message, State) -> {noreply, State}.


%% @doc termination handler. Prints an error message, clean the mailbox and let the program exit.
%%
%% The first parameter is the reason of the shutdown (atom)
terminate(_Reason, _State) ->
  lager:error(<<"[BIBI-BOT][intelligence] Shutting down... Reason is: ~p">>, [_Reason]),
  sb_utils:flush(), %% empty mailbox in case there is still a message upon shutdown
  ok.


%% @doc default behavior on code change, don't do anything.
code_change(_OldVersion, State, _Extra) -> {ok, State}.


%%=============================================================================
%% INTERNAL API
%%=============================================================================

%%-----------------------------------------------------------------------------
%% Types of available messages to treat
%%-----------------------------------------------------------------------------

%% @doc handles a bot command. Send to the user an error message upon failure.
%%
%% The first parameter is a text, a channel, and the user that wrote the text on that particular channel.
handle_command({Text, Channel, User}, #state{botId = BotID, botName = BotName, stateOfMind = StateOfMind}) ->
  {ok, Regex} = re:compile(<<"^<@", BotID/binary, "> (?<Command>.+)">>),

  case re:run(Text, Regex, [{capture, all_names, binary}]) of
    {match, [Command]} ->
      case split_on_space(Command) of
        error -> send_message(Channel, <<"Sorry <@", User/binary, ">, I was unable to parse your command.">>);
        [<<"state">>] -> show_state(Channel, StateOfMind);
        List -> handle_command(List, Channel, User, BotName)
      end;
    _Else -> send_unknown_command(Channel, User, BotName)
  end.


%% @doc handles the sentiment process.
%%
%% The first parameter is a text, a channel, and the user that wrote the text on that particular channel.
handle_message({Message, Channel, User}, #state{stateOfMind = StateOfMind} = State) ->
  Result = sb_sentiment_analysis:analyze(Message, User),
  case Result of
    {ok, Sentiment} ->
      NewStateOfMind = add_to_channel_state(Channel, {User, Sentiment}, StateOfMind),
      send_message(Channel, <<"<@", User/binary, "> is ", Sentiment/binary, ".">>),
      State#state{stateOfMind = NewStateOfMind};
    notfound -> State
  end.




%%-----------------------------------------------------------------------------
%% COMMAND HANDLERS
%%-----------------------------------------------------------------------------

%% @doc performs and gets the result of the help command.
%%
%% The first parameter is a text, a channel and a bot name (string, string, string)
handle_command([<<"help">>], Channel, _User, BotName) -> send_help(Channel, BotName);


%% @doc performs and gets the result of the add command.
%%
%% The first parameter is a regex and a sentiment
%%
%% The second parameter is a channel (string)
%%
%% The third parameter is a user (string)
%%
%% The fourth parameter is a bot name (string)
handle_command([<<"add">>, Regex, Sentiment], Channel, _User, _BotName) ->
  {ok, Sentiment} = sb_sentiment_analysis:add({Regex, Sentiment}),
  send_message(Channel, <<"New sentiment recognition added for ", Sentiment/binary>>);


%% @doc performs and gets the result of the delete command.
%%
%% The first parameter is an indexString (int)
%%
%% The second parameter is a channel (string)
%%
%% The third parameter is a user (string)
%%
%% The fourth parameter is a bot name (string)
handle_command([<<"delete">>, IndexString], Channel, User, _BotName) ->
  try binary_to_integer(IndexString) of
    Index ->
      case sb_sentiment_analysis:delete(Index) of
        {ok, {R, S}} -> send_message(Channel, <<R/binary, " -> ", S/binary, "has been deleted">>);
        out_of_bound -> send_message(Channel, <<"<@", User/binary, "> ", (48 + Index), " is out of bound.">>)
      end
  catch
    error:badarg -> send_message(Channel, <<"<@", User/binary, ">, This is not an integer, I can't delete at this index !">>)
  end;


%% @doc performs and gets the result of the delete command.
%%
%% The first parameter is an indexString (int)
%%
%% The second parameter is a channel (string)
%%
%% The third parameter is a user (string)
%%
%% The fourth parameter is a bot name (string)
handle_command([<<"dump">>], Channel, _User, _BotName) ->
  {ok, Rules} = sb_sentiment_analysis:dump(),
  send_message(Channel, sb_list:mapWithIndex(
    fun({R, Sentiment}, Index) -> <<(48 + Index), ". `", R/binary, "` -> ", Sentiment/binary, "\n">> end,
    Rules
  ));


%% @doc performs and gets the result of the insert command.
%%
%% The first parameter is an indexString, a regex and a sentiment
%%
%% The second parameter is a channel (string)
%%
%% The third parameter is a user (string)
%%
%% The fourth parameter is a bot name (string)
handle_command([<<"insert">>, IndexString, Regex, Sentiment], Channel, User, _BotName) ->
  try binary_to_integer(IndexString) of
    Index ->
      case sb_sentiment_analysis:insert({Regex, Sentiment}, Index) of
        ok -> send_message(
          Channel,
          <<"New sentiment recognition inserted for ", Sentiment/binary, " at index ", (48 + Index)>>
        );
        out_of_bound -> send_message_to(Channel, User, <<(48 + Index), " is out of bound">>)
      end
  catch
    error:badarg -> send_invalid_integer(Channel, User, IndexString)
  end;


%% @doc performs and gets the result of the move command.
%%
%% The first parameter are to indexString (int)
%%
%% The second parameter is a channel (string)
%%
%% The third parameter is a user (string)
%%
%% The fourth parameter is a bot name (string)
handle_command([<<"move">>, OldIndexString, NewIndexString], Channel, User, _BotName) ->
  try binary_to_integer(OldIndexString) of
    Index1 ->
      try binary_to_integer(NewIndexString) of
        Index2 ->
          case sb_sentiment_analysis:move(Index1, Index2) of
            ok -> send_message(Channel, <<"Moved rule from index ", (48 + Index1), " to index ", (48 + Index2), ".">>);
            {out_of_bound, Index} -> send_message_to(Channel, User, <<(48 + Index), " is out of bound">>)
          end
      catch
        error:badarg -> send_invalid_integer(Channel, User, NewIndexString)
      end
  catch
    error:badarg -> send_invalid_integer(Channel, User, OldIndexString)
  end;


%% @doc performs and gets the result of the save command.
%%
%% The first parameter is a channel (string)
%%
%% The second parameter is a user (string)
%%
%% The thrid parameter is a bot name (string)
handle_command([<<"save">>], Channel, _User, _BotName) ->
  ok = sb_sentiment_analysis:save(),
  send_message(Channel, <<"Rules successfully saved">>);


%% @doc default behavior
handle_command(_List, Channel, User, BotName) ->
  send_unknown_command(Channel, User, BotName).


%% @doc return the state of the people minds in this channel.
%%
%% The first parameter is a channel (string)
%%
%% The second parameter is a StateOfMind (string)
show_state(Channel, StateOfMind) ->
  case maps:find(Channel, StateOfMind) of
    error -> send_message(Channel, <<"Unfortunately, Only Psychopats are in this Channel. Nobody has a state of mind.">>);
    {ok, Value} ->
      send_message(Channel, lists:map(
        fun({User, Sentiment}) -> <<"<@", User/binary, "> is ", Sentiment/binary, ".\n">> end,
        maps:to_list(Value)
      ))
  end.


%%-----------------------------------------------------------------------------
%% RESPONSES to Slack
%%-----------------------------------------------------------------------------

%% @doc sends an error message
%%
%% The first parameter is a channel (string)
%%
%% The second parameter is a user (string)
%%
%% The thrid parameter is the value (int)
send_invalid_integer(Channel, User, Value) -> send_message_to(
  Channel,
  User,
  <<"'", Value/binary, "' is not a valid integer">>
).


%% @doc sends an message to a user
%%
%% The first parameter is a channel (string)
%%
%% The second parameter is a user (string)
%%
%% The thrid parameter is the Message (string)
send_message_to(Channel, User, Message) -> send_message(
  Channel,
  <<"Sorry <@", User/binary, "> ", Message/binary>>
).


%% @doc sends an error message upon unknown command.
%%
%% The first parameter is a channel (string)
%%
%% The second parameter is a user (string)
%%
%% The thrid parameter is the BotName (string)
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


%% @doc sends an help message.
%%
%% The first parameter is a channel (string)
%%
%% The second parameter is the BotName (string)
send_help(Channel, BotName) -> send_message(
  Channel,
  <<
    "```@", BotName/binary, " COMMAND [PARAMETERS]...",
    "\n\nAvailable Commands:",
    "\n\thelp                           Display this help message",
    "\n\tadd REGEX SENTIMENT            Add a new sentiment recognition at the begining of the list",
    "\n\tdelete INDEX                   Delete the rule at the given index",
    "\n\tdump                           Dump the current rules",
    "\n\tinsert INDEX REGEX SENTIMENT   Add a new sentiment recognition at the given INDEX",
    "\n\tmove OLD_INDEX NEW_INDEX       Move the rule from OLD_INDEX to NEW_INDEX",
    "\n\tsave                           Save the current rules",
    "\n\tstate                          Show the state of mind of peoples in the channel",
    "```"
  >>
).


%%-----------------------------------------------------------------------------
%% HELPERS
%%-----------------------------------------------------------------------------

%% @doc sends a message to everyone on this channel
send_message(Channel, [_Head | _Tail] = List) ->
  send_message(Channel, list_to_binary(List));

send_message(Channel, Message) ->
  slacker_chat:post_message(sb_config:get_slack_token(), Channel, Message, [{as_user, true}]).


%% @doc helper that splits a string when spaces are encountered
split_on_space(String) ->
  Split = re:split(String, <<" ">>),
  join_on_quotation_mark(Split).


%% @doc join a list on quotation marks
join_on_quotation_mark(L) -> join_on_quotation_mark(L, []).

join_on_quotation_mark([], Acc) -> lists:reverse(Acc);

join_on_quotation_mark([<<"\"", Head/binary>> | Tail], Acc) ->
  case binary:last(Head) of
    $" -> join_on_quotation_mark(Tail, [binary:part(Head, 0, byte_size(Head) - 1) | Acc]);
    _Else ->  consume_until_quotation_mark(Tail, Acc, [Head])
  end;

join_on_quotation_mark([Head | Tail], Acc) -> join_on_quotation_mark(Tail, [Head | Acc]).


%% @doc consume a list until a quotation mark is encountered
consume_until_quotation_mark([], _Acc, _Value) -> error;

consume_until_quotation_mark([Head | Tail], Acc, Value) ->
  case binary:last(Head) of
    $" -> join_on_quotation_mark(
      Tail,
      [list_to_binary(lists:reverse([binary:part(Head, 0, byte_size(Head) - 1), <<" ">> | Value])) | Acc]
    );
    _Else -> consume_until_quotation_mark(Tail, Acc, [Head, <<" ">> | Value])
  end.


%% @doc adds a state to a channel.
add_to_channel_state(Channel, {User, Sentiment}, StateOfMind) ->
  ChannelMap = case maps:find(Channel, StateOfMind) of
    error -> maps:from_list([{User, Sentiment}]);
    {ok, Value} -> maps:put(User, Sentiment, Value)
  end,
  maps:put(Channel, ChannelMap, StateOfMind).