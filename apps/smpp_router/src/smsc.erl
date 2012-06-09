-module(smsc).
-behaviour(gen_smsc).

-include("records.hrl").
-include_lib("oserl/include/oserl.hrl").
-compile({parse_transform, logger_transform}).

-export(
	[
		start_link/1
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

start_link(Params)->
	gen_smsc:start_link(?MODULE, Params, []).

init(Params)->
	Id = proplists:get_value(id, Params),
	logger_transform:add_logger(?MODULE, Id),
	[Link] = mnesia:dirty_read(link, Id),
	State = #connection_state{
		link = Link
	},
	Self = self(),
	spawn(
		fun()->
			Result = gen_smsc:listen_start(Self, Link#link.connection_data#connection_data.port, infinity, ?DEFAULT_SMPP_TIMERS),
			gen_smsc:cast(Self, {listen, Result})
		end
	),
	{ok, State}.

%% gen_smsc functions
handle_operation({CmdName, _Session, _Pdu},_From,State)->
	log4erl:debug(smsc_1,"Got operation~p~n",[CmdName]),
	{reply, {ok, [{message_id, integer_to_list(1)}]}, State}.

handle_unbind({unbind, _Session, _Pdu}, _From, State) -> 
    {reply, ok, State}.

handle_bind({_CmdName, _Session, Pdu, _IpAddr}, _From, State)->
	Link = State#connection_state.link,
	ConnectionData = Link#link.connection_data, 
	SystemId = ConnectionData#connection_data.system_id,
	Password = ConnectionData#connection_data.password,
	BindParams = dict:to_list(Pdu),
	BindAuth = {
		proplists:get_value(system_id, BindParams),
		proplists:get_value(password, BindParams)
	},
	case BindAuth of
		{SystemId, Password} ->
			Result = {ok, [{system_id, SystemId},{password, Password}]};
		_ ->
			Result = {error, ?ESME_RINVSYSID, []}
	end,
	log4erl:debug(smsc_1,"Bind attempt with ~p",[Result]),
	{reply, Result, State}.

handle_listen_error(State) ->
    {noreply, State}.

%% gen_server functions
handle_cast({listen, Activity}, State)->
	State2 = State#connection_state{active=Activity},
	{noreply, State2};
handle_cast(_Req, State)->
	{noreply, State}.


handle_call(_, _From, State)->
	{reply, ok, State}.

handle_info(_,State)->
	{noreply, State}.

code_change(_,_,_)->
	{ok}.

terminate(_,_)->
	ok.