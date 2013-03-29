-module(ga_gapi).
-behaviour(gen_server).

-include("ga.hrl").

%% API
-export([start_link/0, call/2, refresh_token/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {access_token,
                %% config values
                key, secret, token, refresh_url, ga_url, timeout, retry
               }).

%%%===================================================================
%%% API
%%%===================================================================
-spec call(amqp_client(), json()) -> {ok, json()} | {error, term()}.
%% @doc Send request to GA and return reply or error
call(From, Msg) ->
    gen_server:call(?MODULE, {call, From, Msg}).

-spec refresh_token() -> ok | {error, term()}.
%% @doc Refresh oauth temporary token
refresh_token() ->
    gen_server:call(?MODULE, refresh_token).

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
    {ok, #state{key=?CFG(app_key),
                secret=?CFG(app_secret),
                token=?CFG(token),
                refresh_url=?CFG(refresh_url),
                ga_url=?CFG(ga_url),
                timeout=?CFG(http_timeout),
                retry=?CFG(http_retry)}}.

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
            lager:debug("New access token retreived"),
            {reply, ok, State#state{access_token=Token}};
        {error, Err} -> {reply, {error, Err}, State}
    end.

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
handle_info(_Info, State) ->
    {noreply, State}.

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
%% @doc Simple http request wrapper. Only 200 code accepted. Retry on client error
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
