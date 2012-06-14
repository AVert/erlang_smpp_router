-module(smpp_router_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, init_db_tables/0]).

-include("records.hrl").
-include("logger.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	init_logger(),
	?DEBUG(?MODULE, "Logger started"),
	init_db_tables(),
	?DEBUG(?MODULE, "Tables initialized"),
	add_dummy_connections(),
	?DEBUG(?MODULE, "Added dummy connections"),
	Sup = smpp_router_sup:start_link(),
	?DEBUG(?MODULE, "Started dummy supervisor with ~p",[Sup]),
	smpp_router_sup:add_smsc([{id,1}]),
	?DEBUG(?MODULE, "Added smsc with id 1"),
	smpp_router_sup:add_smsc([{id,2}]),
	?DEBUG(?MODULE, "Added smsc with id 2"),
	smpp_router_sup:add_esme([{id,3}]),
	?DEBUG(?MODULE, "Added esme with id 3"),
	Sup.

stop(_State) ->
    ok.

init_logger()->
	log4erl:add_logger(?MODULE),
	log4erl:add_file_appender(?MODULE, ?MODULE,{"log", atom_to_list(?MODULE), {size, 100000}, 4, "elog", debug}),
	log4erl:change_format(?MODULE, file, "%Y-%M-%D %T [%L] %l%n"),
	log4erl:add_logger(router),
	log4erl:add_file_appender(router, router,{"log", "router", {size, 100000}, 4, "elog", debug}),
	log4erl:change_format(router, router, "%Y-%M-%D %T [%L] %l%n").

init_db_tables()->
	mnesia:create_schema([node()]),
	mnesia:start(),
	mnesia:create_table(
		link,
		[
			{attributes, record_info(fields, link)},
			{disc_copies, [node()]}
		]
	),
	mnesia:create_table(
		message,
		[
			{attributes, record_info(fields, message)},
			{disc_copies, [node()]}
		]
	),
	mnesia:create_table(
		rule,
		[
			{attributes, record_info(fields, rule)},
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
	mnesia:dirty_write(link, L2),
	L3 = #link{
		id=3, 
		type=out, 
		connection_data = #connection_data{
			bind_type=transceiver, 
			ip={127,0,0,1}, 
			port=2775, 
			system_id="smppclient1",
			password="password"
		}
	},
	mnesia:dirty_write(link, L3),
	R = #rule{
		id = 1,
		in_id = 1,
		type = submit_sm,
		out_id = 3,
		action = drop
	},
	mnesia:dirty_write(rule, R),
	R1 = #rule{
		id = 2,
		in_id = 2,
		type = submit_sm,
		out_id = 3,
		action = pass
	},
	mnesia:dirty_write(rule, R1),
	R2 = #rule{
		id = 3,
		in_id = 3,
		type = deliver_sm,
		action = drop
	},
	mnesia:dirty_write(rule, R2).
