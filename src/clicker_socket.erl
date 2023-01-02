-module(clicker_socket).

%% API
-export([
    set_active/2,
    connect/1,
    close/1,
    send/2,
    send_and_recv/2,
    send_and_recv/3,
    recv/2,
    recv/3
]).

-define(DEFAULT_CONNECT_TIMEOUT, timer:seconds(5)).
-define(DEFAULT_CLICKHOUSE_PORT, 9000).

%%%_* API ======================================================================
connect(Options) ->
    try
        Host = maps:get(host, Options),
        Port = maps:get(port, Options, ?DEFAULT_CLICKHOUSE_PORT),
        {ok, connect(Host, Port, Options)}
    catch
        error:{badkey, K} -> {error, {missing_mandatory_parameter, K}};
        Error -> Error
    end.

close(#{mod := gen_tcp, sock := Sock}) ->
    gen_tcp:close(Sock);
close(#{mod := ssl, sock := Sock}) ->
    ssl:close(Sock).

set_active(Socket, Mode) ->
    setopts(Socket, [{active, Mode}]).

send(#{mod := gen_tcp, sock := Sock}, IoData) ->
    gen_tcp:send(Sock, IoData);
send(#{mod := ssl, sock := Sock}, IoData) ->
    ssl:send(Sock, IoData).

recv(Socket, Timeout) ->
    recv(Socket, 0, Timeout).

recv(#{mod := gen_tcp, sock := Sock}, Len, Timeout) ->
    gen_tcp:recv(Sock, Len, Timeout);
recv(#{mod := ssl, sock := Sock}, Len, Timeout) ->
    ssl:recv(Sock, Len, Timeout).

send_and_recv(Socket, IoData) ->
    send_and_recv(Socket, IoData, 5000).

send_and_recv(#{mod := gen_tcp, sock := Sock}, IoData, Timeout) ->
    ok = inet:setopts(Sock, [{active, false}]),
    ok = gen_tcp:send(Sock, IoData),
    gen_tcp:recv(Sock, 0, Timeout);
send_and_recv(#{mod := ssl, sock := Sock}, IoData, Timeout) ->
    ok = ssl:setopts(Sock, [{active, false}]),
    ok = ssl:send(Sock, IoData),
    ssl:recv(Sock, 0, Timeout).

%%%_* Internal functions =======================================================
connect(Host, Port, Options) ->
    Timeout = maps:get(connect_timeout, Options, ?DEFAULT_CONNECT_TIMEOUT),
    Deadline = erlang:monotonic_time(millisecond) + Timeout,
    %% initial active opt should be 'false' before upgrading to ssl
    ExtraOpts = maps:get(extra_sock_opts, Options, []),
    SockOpts = [{active, false}, binary] ++ ExtraOpts,
    case gen_tcp:connect(Host, Port, SockOpts, Timeout) of
        {ok, Sock} ->
            Socket = #{host => Host, port => Port, sock => Sock},
            init_connection(Socket, Options, Deadline);
        {error, Reason} ->
            throw(Reason)
    end.

init_connection(#{sock := Sock} = Socket0, Options, Deadline) ->
    %% adjusting buffer size as per recommendation at
    %% http://erlang.org/doc/man/inet.html#setopts-2
    %% idea is from github.com/epgsql/epgsql
    {ok, [{recbuf, RecBufSize}, {sndbuf, SndBufSize}]} =
        inet:getopts(Sock, [recbuf, sndbuf]),
    ok = inet:setopts(Sock, [{buffer, max(RecBufSize, SndBufSize)}]),
    SslOpts = maps:get(ssl, Options, false),
    Mod = get_mod(SslOpts),
    Socket1 = Socket0#{mod => Mod, ssl_opts => SslOpts},
    Socket = maybe_upgrade_to_ssl(Socket1, timeout(Deadline)),
    ok = setopts(Socket, [{active, once}]),
    Socket.

timeout(Deadline) ->
    erlang:max(0, Deadline - erlang:monotonic_time(millisecond)).

get_mod(_SslOpts = true) -> ssl;
get_mod(_SslOpts = [_ | _]) -> ssl;
get_mod(_) -> gen_tcp.

setopts(#{mod := gen_tcp, sock := Sock}, Opts) -> inet:setopts(Sock, Opts);
setopts(#{mod := ssl, sock := Sock}, Opts) -> ssl:setopts(Sock, Opts).

maybe_upgrade_to_ssl(#{mod := ssl} = Socket, Timeout) ->
    #{sock := Sock0, ssl_opts := SslOpts} = Socket,
    case ssl:connect(Sock0, SslOpts, Timeout) of
        {ok, Sock} -> Socket#{sock := Sock};
        {error, Reason} -> erlang:error({failed_to_upgrade_to_ssl, Reason})
    end;
maybe_upgrade_to_ssl(Socket, _Timeout) ->
    Socket.
