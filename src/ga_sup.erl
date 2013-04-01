-module(ga_sup).
-behaviour(supervisor).

%% API
-export([start_link/0, start_link/1, start_query/2]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_link(Sup) ->
    supervisor:start_link({local, Sup}, ?MODULE, [Sup]).

%% @doc Start query worker
start_query(Client, JSON) ->
    supervisor:start_child(query_sup, [Client, JSON]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, [%% AMQP driver
                                  {ga_mq, {ga_mq, start_link, []},
                                   permanent, 5000, worker, [ga_mq]},
                                  %% GAPI interface
                                  {ga_gapi, {ga_gapi, start_link, []},
                                   permanent, 5000, worker, [ga_gapi]},
                                  %% Query pool
                                  {query_sup, {?MODULE, start_link, [query_sup]},
                                   permanent, 5000, supervisor, [?MODULE]} ]}};

init([query_sup]) ->
    {ok, { {simple_one_for_one, 0, 1}, [{ga_query, {ga_query, start_link, []},
                                         temporary, 5000, worker, [ga_query]} ]}}.
