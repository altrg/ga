-module(ga_mq).
-behaviour(gen_server).

-include("ga.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

%% API
-export([start_link/0, send/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {chan, queue, outqueue}).

%%%===================================================================
%%% API
%%%===================================================================
-spec send(binary(), binary()) -> ok.
%% @doc Send message to AMQP
send(To, Msg) ->
    gen_server:cast(?MODULE, {send, To, Msg}).

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
    Params = #amqp_params_network{username=?CFGB(amqp_user),
                                 password=?CFGB(amqp_pass),
                                 host=?CFG(amqp_host),
                                 virtual_host=?CFGB(amqp_virtual_host)},
    {ok, Conn} = amqp_connection:start(Params),
    {ok, Chan} = amqp_connection:open_channel(Conn),
    Queue = ?CFGB(amqp_queue),
    Declare = #'queue.declare'{queue=Queue},
    #'queue.declare_ok'{} = amqp_channel:call(Chan, Declare),
    Consume = #'basic.consume'{queue=Queue},
    #'basic.consume_ok'{} = amqp_channel:call(Chan, Consume),
    lager:info("AMQP connected"),
    {ok, #state{chan=Chan, queue=Queue, outqueue=?CFGB(amqp_outqueue)}}.

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
handle_cast({send, To, Msg}, State) ->
    Queue = if To =:= undefined -> State#state.outqueue;
               true -> To
            end,
    lager:debug("Send message to ~s: ~s", [Queue, Msg]),
    amqp_channel:cast(State#state.chan, #'basic.publish'{routing_key=Queue}, #amqp_msg{payload=Msg}),
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
handle_info(#'basic.consume_ok'{}, State) ->
    {noreply, State};

handle_info({#'basic.deliver'{delivery_tag=Tag}, #amqp_msg{props=Props, payload=Msg}}, State) ->
    From = Props#'P_basic'.reply_to,
    amqp_channel:call(State#state.chan, #'basic.ack'{delivery_tag=Tag}),
    lager:debug("Received message from ~s: ~s", [From, Msg]),
    %% @TODO dispatch
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
