-module(smpp_router_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, init_db_tables/0]).

-include("records.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	init_db_tables(),
	add_dummy_connections(),
	log4erl:change_format(file, {}),
    Sup = smpp_router_sup:start_link(),
	smpp_router_sup:add_smsc([{id,1}]),
	smpp_router_sup:add_smsc([{id,2}]),
	Sup.

stop(_State) ->
    ok.

init_db_tables()->
	mnesia:create_schema([node()]),
	mnesia:start(),
	mnesia:create_table(
		link,
		[
			{attributes, record_info(fields, link)},
			{disc_copies, [node()]}
		]
	).

add_dummy_connections()->
	L = #link{
		id=1, 
		type=in, 
		connection_data = #connection_data{
			bind_type=transceiver, 
			ip={0,0,0,0}, 
			port=3700, 
			system_id="test",
			password="test"
		}
	},
	mnesia:dirty_write(link, L),
	L2 = #link{
		id=2, 
		type=in, 
		connection_data = #connection_data{
			bind_type=transceiver, 
			ip={0,0,0,0}, 
			port=3701, 
			system_id="test",
			password="test"
		}
	},
	mnesia:dirty_write(link, L2).