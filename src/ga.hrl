-ifndef(TEST).
-define(D(Term), lager:debug("~240p", [Term])).
-define(D(Fmt, Args), lager:debug(Fmt, Args)).
-else.
-define(D(Term), io:format(user, "~s:~w ~240p\n", [?FILE, ?LINE, Term])).
-define(D(Fmt, Args), io:format(user, "~s:~w "++Fmt++"\n", [?FILE, ?LINE | Args])).
-endif.

-define(CFG(K), ga_app:get_config(K, [unconfigured, K])).
-define(CFGB(K), list_to_binary(?CFG(K))).

-type amqp_client() :: binary() | undefined.
-type json() :: binary().
-type ga_params() :: proplists:proplist().
