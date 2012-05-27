
-module(smpp_router_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	ProcessFun = fun(Element, Acc)->
		io:format("~p",[Element]),
		Acc ++ [Element]
	end,
	TransactionFun = fun()->
		mnesia:foldl(ProcessFun, [], link)
	end,
	mnesia:transaction(TransactionFun),
    {ok, { {one_for_one, 5, 10}, []} }.

