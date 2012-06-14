-module(router).

-export([route/4]).

-include("logger.hrl").
-include("records.hrl").
-include_lib("oserl/include/oserl.hrl").
	
route(Link, CmdName, Pdu, From)->
	PduParams = dict:to_list(Pdu),
	?DEBUG(router, "Link ~p ~p ~p",[Link, CmdName, PduParams]),
	
	EsmClass = proplists:get_value(esm_class, PduParams),
	?DEBUG(router, "Esm class ~p",[EsmClass]),
	
	OldSequenceId = proplists:get_value(sequence_number, PduParams),
	?DEBUG(router, "Old sequence id ~p",[OldSequenceId]),
	
	case EsmClass of
		4 -> route_delivery_receipt(Link, CmdName, PduParams, From);
		_ -> route_default(Link, CmdName, PduParams, From)
	end.
	
route_delivery_receipt(Link, CmdName, PduParams, From)->
	
	io:format("Routing delivery receipt").
	
route_default(Link, CmdName, PduParams, From)->
	#rule{
		action = Action,
		in_id = SourceLinkId,
		out_id = TargetLinkId
	} = get_rule(Link, CmdName),
	io:format("Action is ~p",[Action]),
	case Action of
		pass ->
			ok;
		drop ->
			?DEBUG(router, ""),
			gen_smsc:reply(From, {error, ?ESME_RSYSERR,[]})
	end.
	
get_rule(Link, CmdName)->
	Rules = mnesia:dirty_match_object(#rule{in_id=Link, type = CmdName, _ = '_'}),
	case Rules of
		[] -> #rule{action=drop};
		[Rule] -> Rule;
		[Head|Tail] -> Head
	end.
	
%%find_message_by_id(MsgId)->
%%	ok.
	
%%process_message_text(Pdu)->
%%	Msg = proplists:get_value(short_message,Pdu),
%%	Pattern = "^id:(\\d+) sub:.* done date:(\\d+) stat:(\\w+) err:",
%%	Options = [global,{capture,all_but_first,list}],
%%	Res = 
%%		case re:run(Msg,Pattern,Options) of
%%			{match,[List]} ->
%%				List;
%%			_ ->
%%				[undefined,now(),undefined]
%%		end,
%%	Res.


%%route(Link, CmdName = submit_sm, Pdu, From)->
%%	io:format("Got ~p from ~p~n",[CmdName, Link]),
%%	[Rule] = mnesia:dirty_match_object(#rule{in_id=Link, type = CmdName, _ = '_'}),
%%	#rule{out_id = OutId} = Rule,
%%	ChannelName = list_to_atom("channel_" ++ integer_to_list(OutId)),
%%	
%%	Session = esme:get_session(ChannelName),
%%	
%%	io:format("Session ~p~n",[Session]),
%%	
%%	if 
%%		Session =:= undefined ->
%%			gen_esme:reply(From, {error, 8,[]});
%%		true ->
%%			ok
%%	end,
%%	
%%	PduParams = dict:to_list(Pdu),
%%
%%	OldSequenceNumber = proplists:get_value(sequence_number, PduParams),
%%	
%%	NewPduParams = [Z || {X, _} = Z <- PduParams, X =/= sequence_number],
%%	
%%	{ok,Result} = gen_esme:submit_sm(Session, NewPduParams),
%%	
%%	ResultParams = dict:to_list(Result),
%%	
%%	MessageId = proplists:get_value(message_id, ResultParams),
%%	
%%	Msg = #message{
%%		id = MessageId,
%%		source_link_id = Link,
%%		target_link_id = OutId
%%	},
%%	
%%	io:format("Msg ~p~n",[Msg]),
%%	
%%	mnesia:dirty_write(message, Msg),
%%	
%%	
%%	NewResultParams = lists:map(
%%		fun
%%			({sequence_number, _}) -> 
%%				{sequence_number, OldSequenceNumber};
%%			({Key, Value}) ->
%%				{Key, Value}
%%		end, 
%%		ResultParams
%%	),
%%	
%%	io:format("Result ~p~n",[NewResultParams]),
%%	
%%	gen_esme:reply(From, {ok, NewResultParams}),
%%	
%%	ok;
%%route(_Link, deliver_sm, Pdu, From)->
%%	Params = dict:to_list(Pdu),
%%	
%%	[MessageId, DoneDate, Stat] = process_message_text(Params),
%%	
%%	io:format("Got ~p~n with ~p ~p ~p~n",[Params, MessageId, DoneDate, Stat]),
%%	
%%	Msg = mnesia:dirty_read(message, MessageId),
%%	
%%	case Msg of
%%		[] -> gen_esme:reply(From, {ok, []});
%%		_ -> ok
%%	end,
%%	
%%	#message{source_link_id = Id} = Msg,
%%	
%%	ChannelAtom = list_to_atom("channel_"++integer_to_list(Id)),
%%	
%%	Res1 = gen_smsc:deliver_sm(ChannelAtom, Params),
%%	
%%	io:format("Reply with ~p~n",[Res1]),
%%	
%%	Res = gen_esme:reply(From, {ok, []}),
%%	
%%	io:format("Reply with ~p~n",[Res]),
%%	ok.




send_packet(smsc, esme, Session, submit_sm, Params)->
	gen_esme:submit_sm(Session, Params);
send_packet(smsc, smsc, Session, submit_sm, Params)->
	gen_smsc:deliver_sm(Session, Params);
send_packet(esme, esme, Session, deliver_sm, Params)->
	gen_esme:submit_sm(Session, Params);
send_packet(esme, smsc, Session, deliver_sm, Params)->
	gen_esme:deliver_sm(Session, Params).


