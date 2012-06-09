
-module(smpp_router_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, add_smsc/1, add_esme/1, add_child/2]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
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


add_smsc(Params)->
	add_child(smsc, Params).

add_esme(Params)->
	add_child(esme, Params).

add_child(Type, Params)->
	Id = proplists:get_value(id, Params),
	Name = list_to_atom(atom_to_list(Type)++integer_to_list(Id)),
	supervisor:start_child(?MODULE, ?CHILD_PARAMS(Name, Type, worker, [Params])).
	