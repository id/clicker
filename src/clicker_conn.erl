-module(clicker_conn).

-include_lib("kernel/include/logger.hrl").
-include("protocol.hrl").

-behaviour(gen_statem).

%% API
-export([
    start_link/1,
    stop/1,
    ping/2,
    query/3
]).

%% gen_statem states
-export([connected/3]).

%% gen_statem callbacks
-export([
    callback_mode/0,
    init/1,
    terminate/3
]).

%%%_* API ======================================================================
start_link(Args) ->
    gen_statem:start_link(?MODULE, Args, []).

stop(ServerRef) ->
    gen_statem:stop(ServerRef).

ping(ServerRef, Timeout) ->
    gen_statem:call(ServerRef, {ping, Timeout}).

query(ServerRef, Query, Timeout) ->
    gen_statem:call(ServerRef, {query, Query, Timeout}).

%%%_* gen_statem callbacks =====================================================
init(Args) ->
    {ok, Socket} = clicker_socket:connect(Args),
    Hello = clicker_protocol:encode_hello(Args),
    {ok, HelloResp} = clicker_socket:send_and_recv(Socket, Hello),
    ServerHello = clicker_protocol:decode_hello(HelloResp),
    ServerRevision = maps:get(server_revision, ServerHello),

    ?LOG_INFO(
        "Connected to ~p server version ~p.~p.~p.~p",
        [
            binary_to_list(maps:get(server_name, ServerHello)),
            maps:get(server_version_major, ServerHello),
            maps:get(server_version_minor, ServerHello),
            maps:get(server_version_patch, ServerHello),
            ServerRevision
        ]
    ),

    case clicker_protocol:encode_addendum(ServerRevision, Args) of
        {ok, Addendum} -> clicker_socket:send(Socket, Addendum);
        _ -> ok
    end,
    {ok, connected, #{
        server_revision => ServerRevision,
        server_hello => ServerHello,
        socket => Socket,
        requests => maps:new()
    }}.

callback_mode() -> [state_functions].

terminate(_Reason, _State, #{socket := Socket}) ->
    clicker_socket:close(Socket).

%%%_* Internal functions =======================================================
connected({call, From}, {ping, Timeout}, #{socket := Socket} = Data) ->
    Ping = clicker_protocol:encode_ping(),
    ok = clicker_socket:set_active(Socket, false),
    ok = clicker_socket:send(Socket, Ping),
    case wait_for_pong(Socket, Timeout) of
        {ok, pong} ->
            {keep_state, Data, {reply, From, pong}};
        {error, timeout} ->
            {keep_state, Data, {reply, From, pang}};
        {error, Reason} ->
            {stop_and_reply, Reason, {reply, From, pang}}
    end;
connected({call, From}, {query, Query, Timeout}, #{socket := Socket} = Data) ->
    #{server_revision := ServerRevision, server_hello := ServerHello} = Data,
    QueryBin = clicker_protocol:encode_query(Query, ServerRevision, ServerHello),
    Block = clicker_protocol:encode_empty_block(),
    ok = clicker_socket:setopts(Socket, [{active, false}, {send_timeout, 5000}]),
    ok = clicker_socket:send(Socket, QueryBin),
    ok = clicker_socket:send(Socket, clicker_protocol:encode_data(Block)),
    case clicker_socket:recv(Socket, 0, Timeout) of
        {ok, Packet} ->
            {keep_state, Data, {reply, From, {ok, Packet}}};
        Error ->
            {keep_state, Data, {reply, From, {error, Error}}}
    end.

%% FIXME: potentially using the same timeout repeatedly
wait_for_pong(Socket, Timeout) ->
    %% Could receive late packets with progress
    case clicker_socket:recv(Socket, Timeout) of
        {ok, Packet} ->
            case clicker_protocol:decode_pong(Packet) of
                ?SERVER_PONG ->
                    {ok, pong};
                ?SERVER_PROGRESS ->
                    wait_for_pong(Socket, Timeout);
                _ ->
                    {error, {unexpected_packet, Packet}}
            end;
        Error ->
            {error, Error}
    end.
