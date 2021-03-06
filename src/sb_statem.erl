%%%-------------------------------------------------------------------
%%% @author Benjamin Schubert, Basile Vu, Ioannis Noukakis and Sarra Berich
%%% @doc
%%% FIXME
%%% @end
%%%-------------------------------------------------------------------
-module(sb_statem).
-behavior(gen_statem).

%% public api
-export([start_link/0, stop/0]).

%% gen_statem callbacks =
-export([callback_mode/0, init/1]).

%% states
-export([disconnected/3, get_websocket_url/3, connect_websocket/3, handle_message/3]).


-record(state, {
  httpPid,
  wsPid,
  wsUrl
}).


-define(SLACK_URL, "slack.com").

%%====================================================================
%% OTP API
%% FIXME: document
%%====================================================================

start_link() ->
  gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).


stop() ->
  gen_statem:stop(?MODULE).


callback_mode() ->state_functions.


init(_Settings) ->
  process_flag(trap_exit, true),
  gen_statem:cast(self(), connect),
  {ok, disconnected, #state{}}.


%%====================================================================
%% STATES
%%====================================================================

%% @doc Start a connection to Slack's servers and move to "get_websocket_url".
disconnected(cast, connect, State) ->
  {ok, HttpPid} = gun:open(?SLACK_URL, 443),
  {next_state, get_websocket_url, State#state{httpPid = HttpPid}, [{state_timeout, 10000, "Could not contact Slack Servers"}]}.


%% @doc The connection to Slack's servers has timed out. Clean resources and die.
get_websocket_url(state_timeout, Message, #state{httpPid = HttpPid}) ->
  gun:shutdown(HttpPid),
  die(Message);


%% @doc The connection to Slack's servers was successful. Get a websocket url.
get_websocket_url(info, {gun_up, HttpPid, _Method}, #state{httpPid = HttpPid} = State) ->
  gun:post(HttpPid, "/api/rtm.connect?token=" ++ sb_config:get_slack_token(), []),
  {keep_state, State};


%% @doc The request to get the websocket's url is ingoing. Continue reading it.
get_websocket_url(info, {gun_response, _HttpPid, _StreamRef, nofin, _Status, _Headers}, State) ->
  %% we consume data until we have the body
  {keep_state, State};


%% @doc The request to get the websocket's url is finished. let's connect and move to state "connect_websocket".
get_websocket_url(info, {gun_data, HttpPid, _StreamRef, fin, Body}, State) ->
  {ok, Url} = maps:find(url, decode(Body)),
  case Url of
    false -> die("Could not get Url of websocket to connect to. Disconnecting.");
    _Else ->
      {ok, {wss, _, Hostname, 443, _Path, _}} = http_uri:parse(binary_to_list(Url), [{scheme_defaults, [{wss, 443}]}]),
      % We have everything, we can close the old connection
      gun:shutdown(HttpPid),
      {ok, WsPid} = gun:open(Hostname, 443),
      {next_state, connect_websocket, State#state{wsPid = WsPid, wsUrl = Url},
        [{state_timeout, 10000, "Could not establish a websocket connection with Slack's Servers"}]
      }
  end.


%% @doc We have a http connection. Upgrade the connection to a websocket connection.
connect_websocket(info, {gun_up, WsPid, _Method}, #state{wsPid = WsPid, wsUrl = WsUrl} = State) ->
  gun:ws_upgrade(WsPid, WsUrl, []),
  {keep_state, State};


%% @doc We have a working websocket connection. Now waiting for the websocket to be active.
connect_websocket(info, {gun_ws_upgrade, _WsPid, ok, _Headers}, State) ->
  {keep_state, State};


%% @doc We check that the first message is indeed a "hello" as stated by Slack's documentation
connect_websocket(info, {gun_ws, _WsPid, {text, Body}}, State) ->
  {ok, <<"hello">>} =  maps:find(type, decode(Body)),
  lager:info("Connection to Slack's servers done. Listening for messages."),
  {next_state, handle_message, State}.


%% @doc Handle the messages coming from the websocket.
handle_message(info, {gun_ws, _WsPid, {text, Body}}, State) ->
  sb_bot_intelligence:handle(Body),
  {keep_state, State}.


%% INTERNAL API

%% @doc decode a json message.
decode(Message) -> jsx:decode(Message, [return_maps, {labels, atom}]).


%% @doc kill the processus.
die(Message) -> exit(whereis(?MODULE), Message).


%% @doc cleanup resources on termination.
terminate(_Reason, #state{wsPid = WsPid, wsUrl = WsUrl} = State, _Data) ->
  lager:error(<<"[BIBI-BOT][statem] Shutting down... Reason is: ~p">>, [_Reason]),
  gun:shutdown(WsPid),
  sb_utils:flush(), %% empty mailbox in case there is still a message upon shutdown
  ok.