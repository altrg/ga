-module(ga_gapi).
-behaviour(gen_server).

-include("ga.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([start_link/0, refresh_token/0, request/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {access_token= <<>>, % refreshable google access token
                requests=dict:new(), % http request / gen_server from refs pairs
                shaper=[], % fixed-size list of timestamps
                %% config values
                key, secret, token, refresh_url, request_url, timeout, retry, qps
               }).

%%%===================================================================
%%% API
%%%===================================================================
-spec refresh_token() -> ok | {error, term()}.
%% @doc Refresh oauth temporary token
refresh_token() ->
    gen_server:call(?MODULE, refresh_token).

-spec request(ga_params()) -> term() | {error, term()}.
%% @doc Make request to GA
request(Params) ->
    gen_server:call(?MODULE, {request, Params}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    ssl:start(),
    {ok, #state{key=?CFG(app_key),
                secret=?CFG(app_secret),
                token=?CFG(token),
                refresh_url=?CFG(refresh_url),
                request_url=?CFG(request_url),
                timeout=?CFG(http_timeout),
                retry=?CFG(http_retry),
                qps=?CFG(qps)}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(refresh_token, _From, State) ->
    URL = State#state.refresh_url,
    Headers = [],
    CType = "application/x-www-form-urlencoded",
    Body = list_to_binary(["client_id=", State#state.key,
                           "&client_secret=", State#state.secret,
                           "&refresh_token=", State#state.token,
                           "&grant_type=refresh_token"]),
    case http(post, {URL, Headers, CType, Body}, State) of
        {ok, JSON} ->
            {Resp} = jiffy:decode(JSON),
            Token = proplists:get_value(<<"access_token">>, Resp),
            lager:debug("New access token retrieved"),
            {reply, ok, State#state{access_token=Token}};
        {error, Err} -> {reply, {error, Err}, State}
    end;

handle_call({request, Params}, From, #state{requests=Reqs, shaper=Shaper}=State) ->
    URL = State#state.request_url++binary_to_list(make_query(Params)),
    Headers = [{"Authorization", "OAuth "++binary_to_list(State#state.access_token)}],
    Now = timestamp(),
    %% queries per second shaper
    if length(Shaper) < State#state.qps -> Shaper1 = [Now | Shaper];
       true ->
            {Shaper1, [Oldest]} = lists:split(State#state.qps, [Now | Shaper]),
            Diff = Now - Oldest,
            if Diff < 1000, Diff > 0 ->
                    lager:warning("Delay request for ~b ms", [Diff]),
                    timer:sleep(Diff);
               true -> ok
            end
    end,
    {ok, Req} = httpc:request(get, {URL, Headers}, [{timeout, State#state.timeout}],
                              [{sync, false}, {full_result, false}]),
    Reqs1 = dict:store(Req, From, Reqs),
    {noreply, State#state{requests=Reqs1, shaper=Shaper1}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({http, {Id, Res}}, #state{requests=Reqs}=State) ->
    gen_server:reply(dict:fetch(Id, Reqs), Res),
    Reqs1 = dict:erase(Id, Reqs),
    {noreply, State#state{requests=Reqs1}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% @doc Simple sync http request wrapper. Only 200 code accepted. Retry on client error
http(Method, Request, State) -> http(Method, Request, State, State#state.retry).
http(Method, Request, State, Attempt) -> 
    case httpc:request(Method, Request, [{timeout, State#state.timeout}], [{full_result, false}]) of
        {ok, {200, Body}} -> {ok, Body};
        {ok, Err} -> {error, Err};
        {error, Err} when Attempt > 1 ->
            lager:warning("Retrying because: ~p", [Err]),
            http(Method, Request, State, Attempt-1);
        {error, Err} -> {error, Err} % no more attempts
    end.

%% @doc Make urlencoded binary query from GA params
make_query([]) -> <<>>;
make_query(Params) -> make_query(Params, []).
make_query([], Acc) -> join(Acc, "&");
make_query([{Par, Val} | T], Acc) -> make_query(T, [[Par, "=", encode(Val)] | Acc]).

%% @doc encode GA value
encode(Val) when is_list(Val) -> encode(join(Val, ","));
encode(Val) -> list_to_binary(http_uri:encode(binary_to_list(Val))).

%% @doc Join list elements with divider
join([H|T], Div) -> list_to_binary([H| [[Div, E] || E <- T]]).

%% @doc Current timestamp, ms
timestamp() ->
    {M, S, Ms} = os:timestamp(), % no need to be as accurate as erlang:now/0 here
    (M * 1000000 + S) * 1000 + Ms div 1000.

%%%===================================================================
%%% Unit tests
%%%===================================================================
make_query_test_() ->
    [?_assertEqual(make_query([]), <<>>),
     ?_assertEqual(make_query([{<<"ids">>, [<<"ga:52415084">>]}]), <<"ids=ga%3A52415084">>),
     ?_assertEqual(make_query([{<<"start-date">>, <<"2013-03-23">>},
                               {<<"end-date">>, <<"2013-03-28">>},
                               {<<"metrics">>, [<<"ga:goal6Completions">>, <<"ga:goal1Completions">>]},
                               {<<"dimensions">>, [<<"ga:landingPagePath">>, <<"ga:date">>]},
                               {<<"ids">>, [<<"ga:52415084">>]}]),
                   <<"ids=ga%3A52415084&",
                     "dimensions=ga%3AlandingPagePath%2Cga%3Adate&",
                     "metrics=ga%3Agoal6Completions%2Cga%3Agoal1Completions&",
                     "end-date=2013-03-28&",
                     "start-date=2013-03-23">>)].
