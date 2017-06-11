%%%-------------------------------------------------------------------
%%% @author Benjamin Schubert, Basile Vu, Ioannis Noukakis and Sarra Berich
%%% @doc
%%% Sentibot Top Level supervisor
%%% @end
%%%-------------------------------------------------------------------

-module(sentibot_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).


%%====================================================================
%% API functions
%%====================================================================

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%%====================================================================
%% Supervisor callbacks
%%====================================================================

-spec init(Args :: term()) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init(_Args) ->
  application:ensure_all_started(gun),
  application:ensure_all_started(slacker),

  SupFlags = #{strategy => one_for_one, intensity => 0, period => 5},

  {ok, {SupFlags, [
    child(sb_sentiment_analysis),
    child(sb_bot_intelligence),
    child(sb_statem)
  ]}}.


%% private API
child(Module) ->
  {Module, {Module, start_link, []}, permanent, 2000, worker, [Module]}.