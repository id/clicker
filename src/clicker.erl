-module(clicker).

%% API
-export([
    close/1,
    connect/1,
    ping/1,
    query/2,
    query/3
]).

%%%_* API ======================================================================
connect(Options) ->
    clicker_conn:start_link(Options).

ping(Conn) ->
    clicker_conn:ping(Conn, 5000).

query(Conn, Query) ->
    query(Conn, Query, 5000).

query(Conn, Query, Timeout) ->
    clicker_conn:query(Conn, Query, Timeout).

close(Conn) ->
    clicker_conn:stop(Conn).
