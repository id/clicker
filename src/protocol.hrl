-ifndef(PROTOCOL_HRL_).
-define(PROTOCOL_HRL_, true).

%% ClickHouse/src/Core/Protocol.h

%% Protocol::Server
%% Name, version, revision.
-define(SERVER_HELLO, 0).
%% A block of data (compressed or not).
-define(SERVER_DATA, 1).
%% The exception during query execution.
-define(SERVER_EXCEPTION, 2).
%% Query execution progress: rows read, bytes read.
-define(SERVER_PROGRESS, 3).
%% Ping response
-define(SERVER_PONG, 4).
%% All packets were transmitted
-define(SERVER_END_OF_STREAM, 5).
%% Packet with profiling info.
-define(SERVER_PROFILE_INFO, 6).
%% A block with totals (compressed or not).
-define(SERVER_TOTALS, 7).
%% A block with minimums and maximums (compressed or not).
-define(SERVER_EXTREMES, 8).
%% A response to TablesStatus request.
-define(SERVER_TABLE_STATUS_RESPONSE, 9).
%% System logs of the query execution
-define(SERVER_LOG, 10).
%% Columns' description for default values calculation
-define(SERVER_TABLE_COLUMNS, 11).
%% List of unique parts ids.
-define(SERVER_PART_UUIDS, 12).
%% String (UUID) describes a request for which next task is needed.
%% This is such an inverted logic, where server sends requests
%% and client returns back response
-define(SERVER_READ_TASK_REQUEST, 13).
%% Packet with profile events from server.
-define(SERVER_PROFILE_EVENTS, 14).
%% Request from a MergeTree replica to a coordinator
-define(SERVER_MERGE_TREE_READ_TASK_REQUEST, 15).
-define(SERVER_MAX, ?CH_SERVER_MERGE_TREE_READ_TASK_REQUEST).

%% Protocol::Client
%% Name, version, revision, default DB
-define(CLIENT_HELLO, 0).
%% Query id, query settings, stage up to which the query must be executed,
%% whether the compression must be used,
%% query text (without data for INSERTs).
-define(CLIENT_QUERY, 1).
%% A block of data (compressed or not).
-define(CLIENT_DATA, 2).
%% Cancel the query execution.
-define(CLIENT_CANCEL, 3).
%% Check that connection to the server is alive.
-define(CLIENT_PING, 4).
%% Check status of tables on the server.
-define(CLIENT_TABLES_STATUS_REQUEST, 5).
%% Keep the connection alive
-define(CLIENT_KEEP_ALIVE, 6).
%% A block of data (compressed or not).
-define(CLIENT_SCALAR, 7).
%% List of unique parts ids to exclude from query processing
-define(CLIENT_IGNORED_PART_UUIDS, 8).
-define(CLIENT_READ_TASK_RESPONSE, 9).
-define(CLIENT_MERGE_TREE_READ_TASK_RESPONSE, 10).
-define(CLIENT_MAX, ?CH_MERGE_TREE_READ_TASK_RESPONSE).

%% ClickHouse/src/Core/BaseSettings.h
-define(SETTINGS_WRITE_FORMAT_BINARY, 0).
-define(SETTINGS_WRITE_FORMAT_STRINGS_WITH_FLAGS, 1).
-define(SETTINGS_WRITE_FORMAT_DEFAULT, ?CH_SETTINGS_WRITE_FORMAT_STRINGS_WITH_FLAGS).

%% ClickHouse/src/Interpreters/ClientInfo.h
-define(QUERY_KIND_NO_QUERY, 0).
-define(QUERY_KIND_INITIAL_QUERY, 1).
-define(QUERY_KIND_SECONDARY_QUERY, 2).

%% ClickHouse/src/Core/QueryProcessingStage.h

%% Only read/have been read the columns specified in the query.
-define(QUERY_STAGE_FETCH_COLUMNS, 0).
%% Until the stage where the results of processing on different servers can be combined.
-define(QUERY_STAGE_WITH_MERGEABLE_STATE, 1).
%% Completely.
-define(QUERY_STAGE_COMPLETE, 2).
%% Until the stage where the aggregate functions were calculated and finalized.
%% It is used for auto distributed_group_by_no_merge optimization for distributed engine.
-define(QUERY_STAGE_WITH_MERGEABLE_STATE_AFTER_AGGREGATION, 3).
%% Same as WithMergeableStateAfterAggregation but also will apply limit on each shard.
%% This query stage will be used for auto
%% distributed_group_by_no_merge/distributed_push_down_limit
%% optimization.
-define(QUERY_STAGE_WITH_MERGEABLE_STATE_AFTER_AGGREGATION_AND_LIMIT, 4).

-define(DBMS_VERSION_MAJOR, 22).
-define(DBMS_VERSION_MINOR, 8).
-define(DBMS_VERSION_PATCH, 0).

-define(CLIENT_NAME, "clicker").
-define(DEFAULT_DATABASE, "").
-define(DEFAULT_USER, "default").
-define(DEFAULT_PASSWORD, "").

-define(INTERFACE_TCP, 1).

-endif.
