# Implementing ClickHouse native TCP protocol

Following the [Native Interface (TCP) implementation guide](https://clickhouse.com/docs/en/interfaces/tcp):

> Unfortunately, native ClickHouse protocol does not have formal specification yet, but it can be reverse-engineered from ClickHouse source code (starting [around here](https://github.com/ClickHouse/ClickHouse/tree/master/src/Client)) and/or by intercepting and analyzing TCP traffic.

## source code

```sh
$ git clone git@github.com:ClickHouse/ClickHouse.git
```

Some files of interest:

- `src/Client/Connection.cpp`
- `src/Client/ClientBase.cpp`
- `programs/client/Client.cpp`
- `src/IO/WriteHelpers.h`
- `src/Formats/NativeWriter.h`
- `src/Formats/NativeWriter.cpp`
- `src/Core/Protocol.h`
- `src/Core/ProtocolDefines.h`
- `src/Core/BlockInfo.h`
- `src/Core/BlockInfo.cpp`
- `src/Core/Block.h`
- `src/Core/Block.cpp`
- `src/Interpreters/ClientInfo.h`
- `src/Interpreters/ClientInfo.cpp`

## tcpdump

### client and server hello

terminal #1
```sh
$ clickhouse server
```

terminal #2
```sh
$ sudo tcpdump -i lo0 -w clickhouse.pcap tcp dst port 9000
```

terminal #3
```sh
$ clickhouse client
```

### query

```sh
$ clickhouse client --query "create table t1 (id String, data String) engine Memory"
$ clickhouse client --query "insert into t1 (*) values ('1', 'data1')"
$ sudo tcpdump -i lo0 -w clickhouse.pcap tcp dst port 9000
$ clickhouse client --query "select * from t1"
```
