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
  {ok, Regex} = re:compile(list_to_binary([<<"^<@">>, BotID, <<"> (?<Command>.*)">>])),
  Result = re:run(Text, Regex, [{capture, all_names, binary}]),

  case Result of
    {match, [Command]} ->
      case Command of
        <<"help">> -> send_help(Channel, BotName);
        _Else -> send_unknown_command(Channel, User, BotName)
      end;
    _Else -> send_unknown_command(Channel, User, BotName)
  end.


handle_message({Message, Channel, User}, _State) ->
  Result = sb_sentiment_analysis:analyze(Message, User),
  case Result of
    {ok, Sentiment} -> send_message(Channel, Sentiment);
    noop -> ok
  end.


%%-----------------------------------------------------------------------------
%% RESPONSES to Slack
%%-----------------------------------------------------------------------------
send_unknown_command(Channel, User, BotName) -> send_message(
  Channel,
  list_to_binary([
    <<"Sorry <@">>,
    User,
    <<">, I could not understand. Correct format is '@">>,
    BotName,
    <<" <command>'.\nPlease use '@">>,
    BotName,
    <<" help' for more information.">>
  ])
).

send_help(Channel, BotName) -> send_message(
  Channel,
  list_to_binary([
    <<"@">>,
    BotName,
    <<" command <parameters>\n\n Available Commands:">>,
    help_fm(<<"help">>, <<"Display this help message">>)
  ])
).


%%-----------------------------------------------------------------------------
%% HELPERS
%%-----------------------------------------------------------------------------
help_fm(Name, Msg) -> list_to_binary([<<"\n\t">>, Name, <<"\t\t">>, Msg]).

send_message(Channel, Message) ->
  slacker_chat:post_message(sb_config:get_slack_token(), Channel, Message, [{as_user, true}]).
