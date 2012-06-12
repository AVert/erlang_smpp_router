%% Logging macroses

-define(LOG(Logger, Level, Msg, Data), log4erl:log(Logger, Level, Msg, Data)).

-define(WARN(Logger, Msg, Data), ?LOG(Logger, warn, Msg, Data)).
-define(WARN(Logger, Msg), ?LOG(Logger, warn, Msg, [])).

-define(ERROR(Logger, Msg, Data), ?LOG(Logger, error, Msg, Data)).
-define(ERROR(Logger, Msg), ?LOG(Logger, error, Msg, [])).

-define(INFO(Logger, Msg, Data), ?LOG(Logger, info, Msg, Data)).
-define(INFO(Logger, Msg), ?LOG(Logger, info, Msg, [])).

-define(DEBUG(Logger, Msg, Data), ?LOG(Logger, debug, Msg, Data)).
-define(DEBUG(Logger, Msg), ?LOG(Logger, debug, Msg, [])).