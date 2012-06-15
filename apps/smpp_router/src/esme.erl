-module(esme).
-behaviour(gen_esme).

-include("records.hrl").
-include("logger.hrl").

-export(
	[
		start_link/1,
		get_session/1,
		bind/3
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
		handle_alert_notification/2,
		handle_outbind/3,
		handle_enquire_link_failure/2,
	 	handle_operation/3,
		handle_unbind/3,
		handle_listen_error/1
	]
).

start_link(Params)->
	Id = proplists:get_value(id, Params),
	Name = lists:flatten(["channel_", integer_to_list(Id)]),
	NameA = list_to_atom(Name),
	gen_esme:start_link({local, NameA},?MODULE, Params, []).

get_session(Pid)->
	gen_esme:call(Pid, get_session).

init(Params)->
	Id = proplists:get_value(id, Params),
	Logger = logger_transform:add_logger(?MODULE, Id),
	[Link] = mnesia:dirty_read(link, Id),
	?DEBUG(Logger,"Link options ~p",[Link]),
	State = #connection_state{
		link = Link,
		logger = Logger
	},
	Self = self(),
	spawn(fun()-> bind(Self, Link, Logger) end),
	{ok, State}.

%% gen_esme functions
handle_alert_notification(_,_)->
	ok.

handle_outbind(_,_,_)->
	ok.

handle_operation({CmdName, _Session, Pdu},From,#connection_state{logger=Logger, link = Link} = State)->
	?DEBUG(Logger, "Got operation ~p with ~p",[CmdName, dict:to_list(Pdu)]),
	#link{id = LinkId} = Link,
	spawn(
		fun()->
			router:route(?MODULE, LinkId, CmdName, Pdu, From)
		end
	),
	{noreply, State}.


handle_unbind(_,_,_)->
	ok.

handle_listen_error(_)->
	ok.

handle_enquire_link_failure(_,_)->
	ok.

%% gen_server functions
handle_cast({bound, Session}, State)->
	#connection_state{
		logger = Logger
	} = State,
	NewState = State#connection_state{active=true, sessions = Session},
	?DEBUG(Logger, "Session pid ~p", [Session]),
	{noreply, NewState};
handle_cast({bind_error, _}, #connection_state{link = Link, logger = Logger} = State)->
	io:format("Self=~p",[self()]),
	Res = timer:apply_after(1000, esme, bind, [self(), Link, Logger]),
	io:format("Timer res ~p",[Res]),
	{noreply, State};
handle_cast(Message, State)->
	#connection_state{
		logger = Logger
	} = State,
	?DEBUG(Logger, "Unknow message ~p",[Message]),
	{noreply, State}.

handle_call(get_session, _From, #connection_state{sessions = Session, active = Active} = State)->
	case Active of
		true ->
			{reply, {Session, ?MODULE}, State};
		_ ->
			{reply, undefined, State}
	end;
handle_call(_, _From, State)->
	io:format("Got default call"),
	{reply, ok, State}.

handle_info(_,State)->
	{noreply, State}.

code_change(_,_,_)->
	{ok}.

terminate(_,_)->
	ok.


bind(Self, Link, Logger) ->
	#link{
		 connection_data = Conection_data
	} = Link,
	#connection_data{
		ip = Host,
		port = Port,
		system_id = System_id,
		password = Password,
		bind_type = Bind_type
	} = Conection_data,	
	?DEBUG(Logger, "Try to start session with ~p:~p",[Host,Port]),
	case gen_esme:session_start(Self, Host, Port) of
		{ok, Tx} ->
			?DEBUG(Logger, "Session started ~p",[Tx]),
 			ParamList = [
				{system_id, System_id},
				{password, Password}
			],
 			?DEBUG(Logger, "Try to bind ~p with ~p", [Bind_type, ParamList]),
			Res = case Bind_type of
				transceiver->
					gen_esme:bind_transceiver(Tx, ParamList);
				receiver->
					gen_esme:bind_receiver(Tx, ParamList);
				transmitter->
					gen_esme:bind_transmitter(Tx, ParamList);
				_ -> throw({error, {illegal_bind_type,Bind_type} })
			end,
			case Res of
				{ok, PduResp} ->
					?INFO(Logger, "Bind session started with resp ~p", [dict:to_list(PduResp)]),
					gen_esme:cast(Self, {bound, Tx});
				BindError ->
					?ERROR(Logger, "Session bind error ~p",[BindError]),
					gen_esme:cast(Self, {bind_error, BindError})
		end;
		SessionError ->
			?ERROR(Logger, "Session error ~p",[SessionError]),
			gen_esme:cast(Self, {bind_error, SessionError})
	end.
