-ifndef(CLICKER_HRL_).
-define(CLICKER_HRL_, true).

%% constants from ClickHouse/src/Core/Protocol.h

%% Protocol.Client
%% Name, version, revision, default DB
-define(CH_CLIENT_HELLO, 0).
%% Query id, query settings, stage up to which the query must be executed,
%% whether the compression must be used,
%% query text (without data for INSERTs).
-define(CH_CLIENT_QUERY, 1).
%% A block of data (compressed or not).
-define(CH_CLIENT_DATA, 2).
%% Cancel the query execution.
-define(CH_CLIENT_CANCEL, 3).
%% Check that connection to the server is alive.
-define(CH_CLIENT_PING, 4).
%% Check status of tables on the server.
-define(CH_CLIENT_TABLES_STATUS_REQUEST, 5).
%% Keep the connection alive
-define(CH_CLIENT_KEEP_ALIVE, 6).
%% A block of data (compressed or not).
-define(CH_CLIENT_SCALAR, 7).
%% List of unique parts ids to exclude from query processing
-define(CH_CLIENT_IGNORED_PART_UUIDS, 8).
-define(CH_CLIENT_READ_TASK_RESPONSE, 9).
-define(CH_CLIENT_MERGE_TREE_READ_TASK_RESPONSE, 10).
-define(CH_CLIENT_MAX, ?CH_MERGE_TREE_READ_TASK_RESPONSE).

%% Protocol.Server
%% Name, version, revision.
-define(CH_SERVER_HELLO, 0).
%% A block of data (compressed or not).
-define(CH_SERVER_DATA, 1).
%% The exception during query execution.
-define(CH_SERVER_EXCEPTION, 2).
%% Query execution progress: rows read, bytes read.
-define(CH_SERVER_PROGRESS, 3).
%% Ping response
-define(CH_SERVER_PONG, 4).
%% All packets were transmitted
-define(CH_SERVER_END_OF_STREAM, 5).
%% Packet with profiling info.
-define(CH_SERVER_PROFILE_INFO, 6).
%% A block with totals (compressed or not).
-define(CH_SERVER_TOTALS, 7).
%% A block with minimums and maximums (compressed or not).
-define(CH_SERVER_EXTREMES, 8).
%% A response to TablesStatus request.
-define(CH_SERVER_TABLE_STATUS_RESPONSE, 9).
%% System logs of the query execution
-define(CH_SERVER_LOG, 10).
%% Columns' description for default values calculation
-define(CH_SERVER_TABLE_COLUMNS, 11).
%% List of unique parts ids.
-define(CH_SERVER_PART_UUIDS, 12).
%% String (UUID) describes a request for which next task is needed.
%% This is such an inverted logic, where server sends requests
%% and client returns back response
-define(CH_SERVER_READ_TASK_REQUEST, 13).
%% Packet with profile events from server.
-define(CH_SERVER_PROFILE_EVENTS, 14).
%% Request from a MergeTree replica to a coordinator
-define(CH_SERVER_MERGE_TREE_READ_TASK_REQUEST, 15).
-define(CH_SERVER_MAX, ?CH_SERVER_MERGE_TREE_READ_TASK_REQUEST).

%% ClickHouse/src/Core/ProtocolDefines.h
-define(CH_SERVER_TCP_PROTOCOL_VERSION, 54460).
-define(CH_SERVER_MIN_REVISION_WITH_SERVER_TIMEZONE, 54058).
-define(CH_SERVER_MIN_REVISION_WITH_DISPLAY_NAME, 54372).
-define(CH_SERVER_MIN_REVISION_WITH_VERSION_PATCH, 54401).
-define(CH_SERVER_MIN_REVISION_WITH_CLIENT_INFO, 54032).
-define(CH_SERVER_MIN_REVISION_WITH_SETTINGS_SERIALIZED_AS_STRINGS, 54429).
-define(CH_SERVER_MIN_REVISION_WITH_INTERSERVER_SECRET, 54441).
-define(CH_SERVER_MIN_PROTOCOL_VERSION_WITH_PARAMETERS, 54459).
-define(CH_SERVER_MIN_PROTOCOL_VERSION_WITH_ADDENDUM, 54458).

%% ClickHouse/src/Core/BaseSettings.h
-define(CH_SETTINGS_WRITE_FORMAT_BINARY, 0).
-define(CH_SETTINGS_WRITE_FORMAT_STRINGS_WITH_FLAGS, 1).
-define(CH_SETTINGS_WRITE_FORMAT_DEFAULT, ?CH_SETTINGS_WRITE_FORMAT_STRINGS_WITH_FLAGS).

%% ClickHouse/src/Interpreters/ClientInfo.h
-define(CH_CLIENT_INFO_QUERY_KIND_NO_QUERY, 0).
-define(CH_CLIENT_INFO_QUERY_KIND_INITIAL_QUERY, 1).
-define(CH_CLIENT_INFO_QUERY_KIND_SECONDARY_QUERY, 2).

-endif.
