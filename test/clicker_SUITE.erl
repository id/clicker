-module(clicker_SUITE).
-compile(export_all).

-export([
    init_per_suite/1,
    end_per_suite/1,
    all/0,
    suite/0
]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%%%_* ct callbacks =============================================================

suite() -> [{timetrap, {seconds, 10}}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

all() ->
    [smoke_test, query].

%%%_* Test functions ===========================================================
smoke_test(Config) when is_list(Config) ->
    {ok, Conn} = clicker:connect(#{host => "localhost"}),
    pong = clicker:ping(Conn),
    ok = clicker:close(Conn).

query(Config) when is_list(Config) ->
    {ok, Conn} = clicker:connect(#{host => "localhost"}),
    {ok, _} = clicker:query(Conn, "create table t1 (id String, data String) engine Memory"),
    {ok, _} = clicker:query(Conn, "insert into t1 (*) values ('1', 'data1')"),
    {ok, _} = clicker:query(Conn, "select * from t1"),
    ok = clicker:close(Conn).
