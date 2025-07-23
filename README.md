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


## Query Results

Query results are automatically decoded into structured data instead of raw binary packets:

- **Data packets**: Include block information such as number of columns and rows
- **Exception packets**: Include error details with code, message, and stack trace  
- **Progress packets**: Include query progress information
- **End of stream packets**: Signal query completion
- **Unknown packet types**: Are handled gracefully

Example decoded results:
```erlang
% Successful query with data
{ok, [{data, #{block_info => #{num_columns => 2, num_rows => 10}}}, end_of_stream]}

% Query with error  
{ok, [{exception, #{code => 404, name => "NotFound", message => "Table not found", stack_trace => ""}}]}

% Query with progress updates
{ok, [{progress, #{read_rows => 1000, read_bytes => 8000, total_rows => 5000}}, 
      {data, #{block_info => #{num_columns => 3, num_rows => 1000}}}, 
      end_of_stream]}
```
