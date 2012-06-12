-module(router).

-export([route/4]).

-include("records.hrl").

route(CmdName, Pdu, Rules, From)->
	[Rule] = 
		lists:filter(
			fun(Rule)->
				#rule{type = Cmd} = Rule,
				case Cmd of
					CmdName ->
						true;
					_ ->
						false
				end
			end, 
			Rules
		),
	#rule{out_id = Out_id} = Rule,
	Name = lists:flatten(["channel_", Out_id]),
	NameAtom = list_to_atom(Name),
	whereis(NameAtom) ! {packet, CmdName, Pdu, From},
	ok.