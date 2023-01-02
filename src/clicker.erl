-module(clicker).

%% API
-export([
    connect/1,
    ping/1
]).

%%%_* API ======================================================================
connect(Options) ->
    clicker_conn:start_link(Options).

ping(Conn) ->
    clicker_conn:ping(Conn, 5000).
