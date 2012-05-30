-module(smsc).
-behaviour(gen_smsc).

-include("records.hrl").

-export(
	[
		start_link/2
	]
).

-export(
	[
		init/1, 
		code_change/3,
		terminate/2,
		handle_cast/2,
		handle_call/3,
		handle_info/2
	]).

-export(
	[
	 	handle_operation/3,
		handle_unbind/3,
		handle_bind/3,
		handle_listen_error/1
	]
).

start_link(Name, Params)->
	gen_smsc:start_link({local, Name}, ?MODULE, Params, []).

init([])->
	State = #connection_state{},
	{ok, State}.

%% gen_smsc functions
handle_operation(_,_,_)->
	ok.

handle_unbind(_,_,_)->
	ok.

handle_bind(_,_,_)->
	ok.

handle_listen_error(_)->
	ok.

%% gen_server functions
handle_cast(_, State)->
	{noreply, State}.

handle_call(_, _From, State)->
	{reply, ok, State}.

handle_info(_,State)->
	{noreply, State}.

code_change(_,_,_)->
	{ok}.

terminate(_,_)->
	ok.