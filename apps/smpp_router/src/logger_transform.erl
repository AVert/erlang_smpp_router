-module(logger_transform).

-export([parse_transform/2, add_logger/2]).

parse_transform(Forms, _Options)->
	%%io:format("~p", [Forms]),
	Forms.

add_logger(Module, Id)->
	Name = atom_to_list(Module) ++ "_" ++ integer_to_list(Id),
	Logger = list_to_atom(Name),
	log4erl:add_logger(Logger),
	log4erl:add_file_appender(Logger, file,{"log", Name, {size, 100000}, 4, "elog", debug}),
	log4erl:change_format(Logger, file, "%Y-%M-%D %T [%L] %l%n").