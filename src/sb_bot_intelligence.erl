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
  token,
  botId,
  botIdPattern
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
  {ok, 200, _Headers, ResponseList} = slacker_auth:test("xoxb-195719487908-v64wxj7ZH0j0rlT3zNf2zIn8"),
  Response = maps:from_list(ResponseList),
  {ok, BotId} = maps:find(<<"user_id">>, Response),
  State = #state{token = "xoxb-195719487908-v64wxj7ZH0j0rlT3zNf2zIn8", botIdPattern = binary:compile_pattern(BotId), botId = BotId},

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
handle_command({Text, Channel, User}, #state{botId = BotID, token = Token} = State) ->
  {ok, Regex} = re:compile(list_to_binary([<<"^<@">>, BotID, <<"> (?<Command>.*)">>])),
  Result = re:run(Text, Regex, [{capture, all_names, binary}]),

  case Result of
    {match, [Command]} ->
      case Command of
        <<"help">> -> send_help(Channel, Token);
        _Else -> send_unknown_command(Channel, User, Token)
      end;
    _Else -> send_unknown_command(Channel, User, Token)
  end.


handle_message({Message, Channel, User}, _State) ->
  lager:info("RECEIVED MESSAGE: ~p", [Message]).


%%-----------------------------------------------------------------------------
%% RESPONSES to Slack
%%-----------------------------------------------------------------------------
send_unknown_command(Channel, User, Token) ->
  slacker_chat:post_message(
    Token,
    Channel,
    list_to_binary([
      <<"Sorry <@">>,
      User,+
      <<">, I could not understand. Correct format is '@bbi-sentibot <command>'.
      Please use @bbi-sentibot help for more information.">>
    ]),
    % FIXME : allow to change name dynamically
    [{username, "BBIS-Sentibot"}]
  ).

send_help(Channel, Token) ->
  slacker_chat:post_message(
    Token,
    Channel,
    list_to_binary([
      <<"@bbi-sentibot command <parameters>\n\n Available Commands:">>,
      help_fm(<<"help">>, <<"Display this help message">>)
    ]),
    % FIXME : allow to change name dynamically
    [{username, "BBIS-Sentibot"}]).


%%-----------------------------------------------------------------------------
%% HELPERS
%%-----------------------------------------------------------------------------
help_fm(Name, Msg) -> list_to_binary([<<"\n\t">>, Name, <<"\t\t">>, Msg]).
