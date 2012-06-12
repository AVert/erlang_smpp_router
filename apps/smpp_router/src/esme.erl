-module(esme).
-behaviour(gen_esme).

-include("records.hrl").
-include("logger.hrl").

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
		handle_alert_notification/2,
		handle_outbind/3,
		handle_enquire_link_failure/2,
	 	handle_operation/3,
		handle_unbind/3,
		handle_listen_error/1
	]
).

start_link(Params)->
	{ok, Pid} = gen_esme:start_link(?MODULE, Params, []),
	Id = proplists:get_value(id, Params),
	Name = lists:flatten(["channel_", Id]),
	register(list_to_atom(Name), Pid),
	{ok, Pid}.

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
	spawn_link(fun()-> bind(Self, Link, Logger) end),
	{ok, State}.

%% gen_esme functions
handle_alert_notification(_,_)->
	ok.

handle_outbind(_,_,_)->
	ok.

handle_operation({Operation, _Session, Pdu},_From,#connection_state{logger = Logger} = State)->
	?DEBUG(Logger, "Got ~p with params ~p",[Operation, dict:to_list(Pdu)]),
	{reply, {ok, []}, State}.

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
handle_cast(Message, State)->
	#connection_state{
		logger = Logger
	} = State,
	?DEBUG(Logger, "Unknow message ~p",[Message]),
	{noreply, State}.

handle_call(_, _From, State)->
	{reply, ok, State}.

handle_info({packet, CmdName, Pdu, From},#connection_state{logger=Logger, sessions=Session} = State)->
	?DEBUG(Logger, "Got ~p",[{packet, CmdName, Pdu}]),
	ParamList = dict:to_list(Pdu),
	{ok, RespDict} = gen_esme:submit_sm(Session, ParamList),
	Resp = dict:to_list(RespDict),
	?DEBUG(Logger, "~p ~p", [From, Resp]),
	gen_server:reply(From,{ok, Resp}),
	{noreply, State};
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