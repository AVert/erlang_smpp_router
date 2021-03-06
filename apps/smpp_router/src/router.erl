-module(router).

-export([route/5]).

-include("logger.hrl").
-include("records.hrl").
-include_lib("oserl/include/oserl.hrl").
	
route(SourceModule, Link, CmdName, Pdu, From)->
	PacketId = mnesia:dirty_update_counter(packet_counter, message_counter, 1),
	?DEBUG(router, "Packet id is ~p", [PacketId]),
	PduParams = dict:to_list(Pdu),
	?DEBUG(router, "Link ~p ~p ~p",[Link, CmdName, PduParams]),
	EsmClass = proplists:get_value(esm_class, PduParams),
	?DEBUG(router, "Esm class ~p",[EsmClass]),
	OldSequenceId = proplists:get_value(sequence_number, PduParams),
	?DEBUG(router, "Old sequence id ~p",[OldSequenceId]),
	case EsmClass of
		4 -> route_delivery_receipt(SourceModule, Link, CmdName, PduParams, From);
		_ -> route_default(SourceModule, Link, CmdName, PduParams, From)
	end.
	
route_delivery_receipt(SourceModule, Link, CmdName, PduParams, From)->
	?DEBUG(router,"Routing delivery receipt"),
	ProcessResult = process_message_text(PduParams),
	case ProcessResult of
		[MsgId, _, _] ->
			?DEBUG(router, "Got msg id ~p",[MsgId]),
			MessageRaw = mnesia:dirty_read(message, MsgId),
			case MessageRaw of
				[Message] ->
					?DEBUG(router, "Got message ~p",[Message]),
					#message{source_link_id = SourceLinkId,target_link_id=TargetLinkId} = Message, 
					route_packet_to_link(SourceModule, TargetLinkId, SourceLinkId, CmdName, PduParams, From);
				_ ->
					?DEBUG(router, "No message with such ID:~p", [MsgId]),
					route_default(SourceModule, Link, CmdName, PduParams, From)
			end;		
		_ ->
			route_default(SourceModule, Link, CmdName, PduParams, From)
	end.
	
	
route_default(SourceModule,Link, CmdName, PduParams, From)->
	#rule{
		action = Action,
		in_id = SourceLinkId,
		out_id = TargetLinkId
	} = get_rule(Link, CmdName),
	?DEBUG(router, "Action ~p",[Action]),
	case Action of
		pass ->
			route_packet_to_link(SourceModule, SourceLinkId, TargetLinkId, CmdName, PduParams, From);
		_ ->
			?DEBUG(router, "Do not pass"),
			gen_esme:reply(From, {ok, [{message_id, integer_to_list(1234)}]})
	end.
	
route_packet_to_link(SourceModule, SourceLinkId, TargetLinkId, CmdName, PduParams, From)->
	Link = list_to_atom("channel_" ++ integer_to_list(TargetLinkId)),
	?DEBUG(router,"Route message to link ~p",[TargetLinkId]),
	LinkRaw = mnesia:dirty_read(link,TargetLinkId),
	?DEBUG(router, "Link raw is ~p",[LinkRaw]),
	[#link{type = Type}] = LinkRaw,
	case Type of
		in ->
			SessionTuple = smsc:get_session(Link);
		out ->
			SessionTuple = esme:get_session(Link)
	end,
	?DEBUG(router, "Session of link ~p",[SessionTuple]),
	case SessionTuple of
		undefined ->
			gen_server:reply(From, {error, ?ESME_RSYSERR,[]});
		{Session, TargetModule} ->
			Result = send_packet(SourceModule, SourceLinkId, TargetModule, TargetLinkId, Session, CmdName, PduParams),
			?DEBUG(router, "Will send result ~p",[Result]),
			case Result of
				{ok, ResultParams} ->
					gen_server:reply(From, {ok, ResultParams});
				{error, Err} ->
					gen_server:reply(From, {error, Err})
			end
	end.		
	
send_packet(SourceModule, SourceLinkId, TargetModule, TargetLinkId ,Session, CmdName, PduParams)->
	OldSequenceNumber = proplists:get_value(sequence_number, PduParams),
	NewPduParams = [Z || {X, _} = Z <- PduParams, X =/= sequence_number],
	?DEBUG(router, "Send ~p with ~p",[CmdName, NewPduParams]),
	Result = send_pdu_packet(SourceModule, TargetModule, Session, CmdName, NewPduParams),
	?DEBUG(router, "Got result ~p",[Result]),
	ResultParams = process_result(SourceLinkId, TargetLinkId, Result),
	BaseResultParams = proplists:delete(sequence_number, ResultParams),
	ResultPduParams = BaseResultParams++[{sequence_number, OldSequenceNumber}],
	case Result of
		{ok, _} ->
			{ok, ResultPduParams};
		{error, Err} ->
			{error, Err}
	end.
	
	
process_result(SourceLinkId, TargetLinkId, {ok, ResultPdu})->
	ResultParams = dict:to_list(ResultPdu),
	save_message(SourceLinkId, TargetLinkId, ResultParams),
	ResultParams;
process_result(_SourceLinkId, _TargetLinkId, {error, _})->
	[].

	
send_pdu_packet(smsc, esme, Session, submit_sm, Params)->
	?DEBUG(router, "smsc to esme with submit_sm ~p",[Params]),
	gen_esme:submit_sm(Session, Params);
send_pdu_packet(smsc, smsc, Session, submit_sm, Params)->
	?DEBUG(router, "smsc to smsc with deliver_sm ~p",[Params]),
	gen_smsc:deliver_sm(Session, Params);
send_pdu_packet(esme, esme, Session, deliver_sm, Params)->
	?DEBUG(router, "esme to esme with submit_sm ~p",[Params]),
	gen_esme:submit_sm(Session, Params);
send_pdu_packet(esme, smsc, Session, deliver_sm, Params)->
	?DEBUG(router, "esme to smsc with deliver_sm ~p",[Params]),
	gen_smsc:deliver_sm(Session, Params).
	
	
save_message(SourceLinkId, TargetLinkId, PduParams)->
	MessageId = proplists:get_value(message_id, PduParams),
	Msg = #message{
		id = MessageId,
		source_link_id = SourceLinkId,
		target_link_id = TargetLinkId
	},
	?DEBUG(router, "Msg ~p",[Msg]),
	ok = mnesia:dirty_write(message, Msg).
	
get_rule(Link, CmdName)->
	Rules = mnesia:dirty_match_object(#rule{in_id=Link, type = CmdName, _ = '_'}),
	case Rules of
		[] -> #rule{action=drop};
		[Rule] -> Rule;
		[Head|_] -> Head
	end.
	
process_message_text(Pdu)->
	Msg = proplists:get_value(short_message,Pdu),
	Pattern = "^id:(\\d+) sub:.* done date:(\\d+) stat:(\\w+) err:",
	Options = [global,{capture,all_but_first,list}],
	Res = 
		case re:run(Msg,Pattern,Options) of
			{match,[List]} ->
				List;
			_ ->
				[undefined,now(),undefined]
		end,
	Res.
