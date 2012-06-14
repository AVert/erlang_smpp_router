-module(logger_transform).

-export([add_logger/2]).

-include("logger.hrl").

add_logger(Module, Id)->
	Name = "logger_" ++ atom_to_list(Module) ++ "_" ++ integer_to_list(Id),
	Logger = list_to_atom(Name),
	{ok, _Pid1} = log4erl:add_logger(Logger),
	{ok, _Pid2} = log4erl:add_file_appender(Logger, Logger,{"log", Name, {size, 100000}, 4, "elog", debug}),
	ok = log4erl:change_format(Logger, Logger, "%Y-%M-%D %T [%L] %l%n"),
	Logger.
