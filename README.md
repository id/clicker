# Clicker - ClickHouse Erlang Driver

ClickHouse Erlang Driver with native (TCP) interface support.

## Quick start

```sh
$ make
$ make test-env-up
$ make ct
$ make test-env-down
```

## Erlang shell

```
$ make shell
1> {ok, Conn} = clicker:connect(#{host => "localhost"}).
...
2> clicker:ping(Conn).
pong
3> clicker:query(Conn, "create table t1 (id String, data String) engine Memory").
...
4> clicker:query(Conn, "insert into t1 (*) values ('1', 'data1')").
...
5> clicker:query(Conn, "select * from t1").
...
```
