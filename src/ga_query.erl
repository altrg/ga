-module(ga_query).
-behaviour(gen_server).

-include("ga.hrl").

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(INVALID_MESSAGE, <<"Unknown message format">>).
-define(ERROR_QUERY, <<"Error sending query to GA">>).
-define(UNKNOWN_RESPONSE, <<"Unhandled response received: ">>).

-record(state, {client, json, params, token_refreshed=false}).

%%%===================================================================
%%% API
%%%===================================================================
-spec start_link(amqp_client(), json()) -> {ok, pid()} | ignore | {error, term()}.
%% @doc Start worker for handling query
start_link(Client, JSON) ->
    gen_server:start_link(?MODULE, [Client, JSON], []).

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
init([Client, JSON]) ->
    self() ! parse,
    {ok, #state{client=Client, json=JSON}}.

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
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

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
handle_info(parse, #state{json=JSON}=State) ->
    try
        {Params} = jiffy:decode(JSON),
        self() ! query_gapi,
        {noreply, State#state{params=Params}}
    catch throw:{error, Err} ->
            lager:error("Invalid message: ~p~n~s", [Err, JSON]),
            ga_mq:send(State#state.client, ?INVALID_MESSAGE),
            {stop, normal, State}
    end;

handle_info(query_gapi, #state{params=Params}=State) ->
    case gapi_request(Params) of
        {error, Err} ->
            lager:error("Can't query GA: ~p", [Err]),
            ga_mq:send(State#state.client, ?ERROR_QUERY),
            {stop, normal, State};
        {200, Body} ->
            ga_mq:send(State#state.client, Body),
            {stop, normal, State};
        {401, _} when not State#state.token_refreshed ->
            lager:warning("Token must be refreshed"),
            ok = ga_gapi:refresh_token(),
            self() ! query_gapi,
            {noreply, State#state{token_refreshed=true}};
        {_, Body} ->
            Reply = list_to_binary([?UNKNOWN_RESPONSE, Body]),
            ga_mq:send(State#state.client, Reply),
            {stop, normal, State}
    end.

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
%% @doc Ask GAPI, Retry on http client error
gapi_request(Params) -> gapi_request(Params, ?CFG(http_retry)).
gapi_request(Params, Attempt) ->
    case ga_gapi:request(Params) of
        {error, Err} when Attempt > 1 ->
            lager:warning("Retrying because: ~p", [Err]),
            gapi_request(Params, Attempt-1);
        {error, Err} -> {error, Err}; % no more attempts
        Reply -> Reply
    end.
