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
	log4erl:change_format(file, {}),
    smpp_router_sup:start_link().

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
