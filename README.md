# ClickHouse client library in Erlang

Erlang library implementing native ClickHouse TCP protocol.

## Build

```sh
$ make
```

## Run

```
$ make shell
1> {ok, Conn} = clicker:connect(#{host => "localhost"}).
{ok,<0.138.0>}
2> clicker:ping(Conn).
pong
```

