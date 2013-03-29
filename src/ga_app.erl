-module(ga_app).
-behaviour(application).

%% API
-export([start/0, get_config/2]).

%% Application callbacks
-export([start/2, stop/1]).

-define(APPS, [lager, inets, ssl]).

%% ===================================================================
%% API
%% ===================================================================
%% @doc Launcher for erl .. -s ga_app ..
start() ->
    application:start(ga, permanent).

%% @doc Get value or default from config
-spec get_config(atom(), term()) -> term().
get_config(Par, Default) ->
    case application:get_env(ga, Par) of
        undefined -> Default;
        {ok, Val} -> Val
    end.

%% ===================================================================
%% Application callbacks
%% ===================================================================
start(_StartType, _StartArgs) ->
    [application:load(App) || App <- ?APPS],
    load_config(),
    [application:start(App) || App <- ?APPS],
    lager:info("Starting ga"),
    ga_sup:start_link().

stop(_State) ->
    ok.

%% ===================================================================
%% Internal functions
%% ===================================================================
%% @doc Init apps from config
load_config() ->
    {ok, [[CfgFile]]} = init:get_argument(cfg),
    {ok, Config} = file:consult(CfgFile),
    [[application:set_env(App, Key, Val) || {Key, Val} <- Cfg]
        || {App, Cfg} <- Config].
