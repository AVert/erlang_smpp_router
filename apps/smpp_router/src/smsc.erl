-module(smsc).
-behaviour(gen_smsc).

-include("records.hrl").
-include_lib("oserl/include/oserl.hrl").
-include("logger.hrl").

-export(
	[
		start_link/1,
		get_session/1
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
	Id = proplists:get_value(id, Params),
	Name = lists:flatten(["channel_", integer_to_list(Id)]),
	NameA = list_to_atom(Name),
	gen_smsc:start_link({local, NameA},?MODULE, Params, []).

get_session(Pid)->
	gen_smsc:call(Pid, get_session).

init(Params)->
	Id = proplists:get_value(id, Params),
	Logger = logger_transform:add_logger(?MODULE, Id),
	[Link] = mnesia:dirty_read(link, Id),
	?DEBUG(Logger,"Link options ~p",[Link]),
	Guard = #rule{id ='_', in_id = Id, out_id = '_', type = '_'},
	Rules = mnesia:dirty_match_object(rule, Guard),
	?DEBUG(Logger, "Got rules ~p",[Rules]),
	State = #connection_state{
		link = Link,
		logger = Logger,
		rules = Rules
	},
	Self = self(),
	spawn(
		fun()->
			Connection_data = Link#link.connection_data,
			Port = Connection_data#connection_data.port,
			Result = gen_smsc:listen_start(Self, Port, infinity, ?DEFAULT_SMPP_TIMERS),
			?DEBUG(Logger, "Start listening on port ~p with result ~p",[Port, Result]),
			gen_smsc:cast(Self, {listen, Result})
		end
	),
	{ok, State}.

%% gen_smsc functions
handle_operation({CmdName, _Session, Pdu},From,#connection_state{logger=Logger, link = Link} = State)->
	?DEBUG(Logger, "Got operation ~p with ~p",[CmdName, dict:to_list(Pdu)]),
	#link{id = LinkId} = Link,
	spawn(
		fun()->
			router:route(?MODULE, LinkId, CmdName, Pdu, From)
		end
	),
	{noreply, State}.

handle_unbind({unbind, _Session, _Pdu}, _From, #connection_state{logger=Logger} = State) -> 
	?INFO(Logger, "Got unbind"),
	NewState = State#connection_state{active=false},
    {reply, ok, NewState}.

handle_bind({_CmdName, _Session, Pdu, _IpAddr}, _From, State)->
	#connection_state{
		link=Link,
		logger = Logger
	} = State,
	#link{
		connection_data = ConnectionData
	} = Link,
	SystemId = ConnectionData#connection_data.system_id,
	Password = ConnectionData#connection_data.password,
	BindParams = dict:to_list(Pdu),
	
	?DEBUG(Logger, "Got bind packet with ~p",[BindParams]),
	
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
	?DEBUG(Logger, "Bind attempt with ~p",[Result]),
	{reply, Result, State}.

handle_listen_error(#connection_state{logger = Logger} =State) ->
	?INFO(Logger, "Got listen error"),
	NewState = State#connection_state{active=false},
    {noreply, NewState}.

%% gen_server functions
handle_cast({listen, Activity}, State)->
	State2 = State#connection_state{active=Activity},
	{noreply, State2};
handle_cast(_Req, State)->
	{noreply, State}.

handle_call(get_session, _From, #connection_state{sessions = Session, active = Active} = State)->
	case Active of
		true ->
			{reply, {Session, ?MODULE}, State};
		_ ->
			{reply, undefined, State}
	end;
handle_call(_, _From, State)->
	{reply, ok, State}.

handle_info(_,State)->
	{noreply, State}.

code_change(_,_,_)->
	{ok}.

terminate(_,_)->
	ok.
