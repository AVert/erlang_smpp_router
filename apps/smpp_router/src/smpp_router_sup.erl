
-module(smpp_router_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, add_smsc/2, add_esme/2]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Name, I, Type), {Name, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(CHILD_PARAMS(Name, I, Type, Params), {Name, {I, start_link, Params}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, []} }.


add_smsc(Name, Params)->
	supervisor:start_child(?MODULE, ?CHILD_PARAMS(Name, esme, worker, [Name,Params])).

add_esme(Name, Params)->
	supervisor:start_child(?MODULE, ?CHILD_PARAMS(Name, esme, worker, [Name,Params])).