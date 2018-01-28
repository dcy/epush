%%%-------------------------------------------------------------------
%% @doc epush top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(epush_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->

    Restart = permanent,
    Shutdown = 6000,
    PoolChild = #{id => epush_pool_sup,
                       start => {epush_pool_sup, start_link, []},
                       restart => Restart, shutdown => Shutdown, type => supervisor,
                       modules => [epush_pool_sup]},
    {ok, { {one_for_all, 10, 10}, [PoolChild]} }.

%%====================================================================
%% Internal functions
%%====================================================================
