
%%%-------------------------------------------------------------------
%%% @author tellendil
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Jun 2017 23:12
%%%-------------------------------------------------------------------
-module(sb_statem).
-behavior(gen_statem).

%% public api
-export([start_link/0, stop/0]).

%% gen_statem callbacks =
-export([callback_mode/0, init/1]).

%% states
-export([disconnected/3, connecting/3, get_websocket_url/3, connect_websocket/3]).


-record(state, {
  httpPid,
  wsPid,
  wsUrl
}).


-define(SLACK_URL, "slack.com").

%%====================================================================
%% OTP API
%%====================================================================
start_link() ->
  gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).


stop() ->
  gen_statem:stop(?MODULE).


callback_mode() ->state_functions.


init(_Settings) ->
  State = #state{},
  gen_statem:cast(self(), connect),
  {ok, disconnected, State}.


%%====================================================================
%% STATES
%%====================================================================

disconnected(cast, connect, State) ->
  {ok, HttpPid} = gun:open(?SLACK_URL, 443),
  {next_state, connecting, State#state{httpPid = HttpPid}};

disconnected(Type, Data, State) -> unhandled_message(disconnected, Type, Data, State).

connecting(info, {gun_up, _, _} = Data, State) ->
  gen_statem:cast(self(), Data),
  {next_state, get_websocket_url, State};


connecting(Type, Data, State) -> unhandled_message(connecting, Type, Data, State).


connect_websocket(info, {gun_up, WsPid, _Method}, #state{wsPid = WsPid, wsUrl = WsUrl} = State) ->
  gun:ws_upgrade(WsPid, WsUrl, []),
  {keep_state, State};

connect_websocket(info, {gun_ws_upgrade, _WsPid, ok, _Headers}, State) ->
  lager:info("CONNECTED"),
  {keep_state, State};

connect_websocket(info, {gun_ws, _WsPid, Message}, State) ->
  lager:info("~p", [Message]),
  {keep_state, State};

connect_websocket(info, {gun_data, _WsPid, Message}, State) ->
  lagger:info("~p", [Message]),
  {keep_state, State};

connect_websocket(Type, Data, State) -> unhandled_message(connecting, Type, Data, State).


get_websocket_url(cast, {gun_up, HttpPid, _Method}, #state{httpPid = HttpPid} = State) ->
  gun:post(HttpPid, "/api/rtm.connect?token=" ++ "xoxb-195719487908-v64wxj7ZH0j0rlT3zNf2zIn8", []),
  {keep_state, State};

get_websocket_url(info, {gun_response, _HttpPid, _StreamRef, nofin, _Status, _Headers}, State) ->
  %% we consume data until we have the body
  {keep_state, State};

get_websocket_url(info, {gun_data, _HttpPid, _StreamRef, fin, Body}, State) ->
  {<<"url">>, Url} = lists:keyfind(<<"url">>, 1, jsx:decode(Body)),
  case Url of
    false ->
      lager:error("Could not get Url of websocket to connect to. Disconnecting."),
      {next_state, disconnected, State};
    _Else ->
      {ok, {wss, _, Hostname, 443, _Path, _}} = http_uri:parse(binary_to_list(Url), [{scheme_defaults, [{wss, 443}]}]),
      {ok, WsPid} = gun:open(Hostname, 443),
      {next_state, connect_websocket, State#state{wsPid = WsPid, wsUrl = Url}}
  end;

get_websocket_url(Type, Data, State) -> unhandled_message(connecting, Type, Data, State).


%% INTERNAL API
unhandled_message(Function, Type, Data, State) ->
  lager:error("Got unhandled message for '~p', type: '~p', message: '~p'.", [Function, Type, Data]),
  %lager:error("State was '~p'", [State]),
  gen_statem:cast(self(), connect),
  {next_state, disconnected, #state{}}.
