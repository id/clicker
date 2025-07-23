-module(clicker_protocol).

-include("protocol_defines.hrl").
-include("protocol.hrl").

-export([
    encode_hello/1,
    decode_hello/1,
    encode_ping/0,
    decode_pong/1,
    encode_addendum/2,
    encode_query/3,
    encode_empty_block/0,
    encode_data/1,
    decode_query_result/1
]).

-define(make_client_name(CLIENT_NAME), (iolist_to_binary(["ClickHouse ", CLIENT_NAME]))).
-define(do_if(REV, MIN_REV, FUN), begin
    (case REV >= MIN_REV of
        true -> FUN;
        _ -> <<>>
    end)
end).

encode_hello(Config) ->
    ClientName = maps:get(client_name, Config, ?CLIENT_NAME),
    Database = maps:get(database, Config, ?DEFAULT_DATABASE),
    User = maps:get(user, Config, ?DEFAULT_USER),
    Password = maps:get(password, Config, ?DEFAULT_PASSWORD),
    [
        clicker_lib:encode_varuint(?CLIENT_HELLO),
        clicker_lib:encode_string(?make_client_name(ClientName)),
        clicker_lib:encode_varuint(?DBMS_VERSION_MAJOR),
        clicker_lib:encode_varuint(?DBMS_VERSION_MINOR),
        clicker_lib:encode_varuint(?DBMS_TCP_PROTOCOL_VERSION),
        clicker_lib:encode_string(Database),
        clicker_lib:encode_string(User),
        clicker_lib:encode_string(Password)
    ].

decode_hello(Bin0) ->
    {?SERVER_HELLO, Bin1} = clicker_lib:decode_varuint(Bin0),
    {ServerName, Bin2} = clicker_lib:decode_string(Bin1),
    {ServerVersionMajor, Bin3} = clicker_lib:decode_varuint(Bin2),
    {ServerVersionMinor, Bin4} = clicker_lib:decode_varuint(Bin3),
    {ServerRevision, Bin5} = clicker_lib:decode_varuint(Bin4),
    {ServerTimezone, Bin6} = maybe_read_server_timezone(Bin5, ServerRevision),
    {ServerDisplayName, Bin7} = maybe_read_server_display_name(Bin6, ServerRevision),
    {ServerVersionPatch, _} = maybe_read_server_version_patch(Bin7, ServerRevision),
    #{
        server_name => ServerName,
        server_version_major => ServerVersionMajor,
        server_version_minor => ServerVersionMinor,
        server_revision => ServerRevision,
        server_version_patch => ServerVersionPatch,
        server_timezone => ServerTimezone,
        server_display_name => ServerDisplayName
    }.

maybe_read_server_timezone(Bin, ServerRevision) when
    ServerRevision >= ?DBMS_MIN_REVISION_WITH_SERVER_TIMEZONE
->
    clicker_lib:decode_string(Bin);
maybe_read_server_timezone(Bin, _) ->
    {"", Bin}.

maybe_read_server_display_name(Bin, ServerRevision) when
    ServerRevision >= ?DBMS_MIN_REVISION_WITH_SERVER_DISPLAY_NAME
->
    clicker_lib:decode_string(Bin);
maybe_read_server_display_name(Bin, _) ->
    {"", Bin}.

maybe_read_server_version_patch(Bin, ServerRevision) when
    ServerRevision >= ?DBMS_MIN_REVISION_WITH_VERSION_PATCH
->
    clicker_lib:decode_varuint(Bin);
maybe_read_server_version_patch(Bin, ServerRevision) ->
    {ServerRevision, Bin}.

encode_ping() ->
    clicker_lib:encode_varuint(?CLIENT_PING).

decode_pong(Bin) ->
    {Pong, _Tail} = clicker_lib:decode_varuint(Bin),
    Pong.

encode_addendum(ServerRevision, Config) when
    ServerRevision >= ?DBMS_MIN_PROTOCOL_VERSION_WITH_ADDENDUM
->
    {ok, clicker_lib:encode_string(maps:get(quota_key, Config, ""))};
encode_addendum(_, _) ->
    false.

encode_query(Query, ServerRevision, Options) ->
    QueryId = maps:get(query_id, Options, ""),
    Parameters = maps:get(parameters, Options, []),
    IoList = [
        clicker_lib:encode_varuint(?CLIENT_QUERY),
        clicker_lib:encode_string(QueryId),
        ?do_if(
            ServerRevision,
            ?DBMS_MIN_REVISION_WITH_CLIENT_INFO,
            encode_client_info(ServerRevision, Options)
        ),
        encode_settings(Options),
        ?do_if(
            ServerRevision,
            ?DBMS_MIN_REVISION_WITH_INTERSERVER_SECRET,
            clicker_lib:encode_string("")
        ),
        clicker_lib:encode_varuint(?QUERY_STAGE_COMPLETE),
        %% TODO: compression
        clicker_lib:encode(boolean, false),
        clicker_lib:encode_string(Query),
        ?do_if(
            ServerRevision,
            ?DBMS_MIN_PROTOCOL_VERSION_WITH_PARAMETERS,
            encode_parameters(Parameters)
        )
    ],
    iolist_to_binary(IoList).

encode_settings(_Options) ->
    %% TODO: encode settings
    clicker_lib:encode_string("").

encode_parameters(_Parameters) ->
    %% TODO: encode parameters
    clicker_lib:encode_string("").

encode_client_info(ServerRevision, Options) ->
    QueryKind = maps:get(query_kind, Options, ?QUERY_KIND_INITIAL_QUERY),
    InitialUser = "",
    InitialQueryId = "",
    InitialAddress = "0.0.0.0:0",
    InitialQueryStartTimeMicroseconds = 0,
    OsUser =
        case os:getenv("USER") of
            false -> "";
            User -> User
        end,
    {ok, ClientHostname} = net_adm:dns_hostname(net_adm:localhost()),
    ClientName = maps:get(client_name, Options, ?CLIENT_NAME),
    QuotaKey = maps:get(quota_key, Options, ""),
    DistributedDepth = maps:get(distributed_depth, Options, 0),
    [
        clicker_lib:encode_varuint(QueryKind),
        clicker_lib:encode_string(InitialUser),
        clicker_lib:encode_string(InitialQueryId),
        clicker_lib:encode_string(InitialAddress),
        ?do_if(
            ServerRevision,
            ?DBMS_MIN_PROTOCOL_VERSION_WITH_INITIAL_QUERY_START_TIME,
            clicker_lib:encode(uint64, InitialQueryStartTimeMicroseconds)
        ),
        clicker_lib:encode(uint8, ?INTERFACE_TCP),
        clicker_lib:encode_string(OsUser),
        clicker_lib:encode_string(ClientHostname),
        clicker_lib:encode_string(?make_client_name(ClientName)),
        clicker_lib:encode_varuint(?DBMS_VERSION_MAJOR),
        clicker_lib:encode_varuint(?DBMS_VERSION_MINOR),
        clicker_lib:encode_varuint(?DBMS_TCP_PROTOCOL_VERSION),
        ?do_if(
            ServerRevision,
            ?DBMS_MIN_REVISION_WITH_QUOTA_KEY_IN_CLIENT_INFO,
            clicker_lib:encode_string(QuotaKey)
        ),
        ?do_if(
            ServerRevision,
            ?DBMS_MIN_PROTOCOL_VERSION_WITH_DISTRIBUTED_DEPTH,
            clicker_lib:encode_varuint(DistributedDepth)
        ),
        ?do_if(
            ServerRevision,
            ?DBMS_MIN_REVISION_WITH_VERSION_PATCH,
            clicker_lib:encode_varuint(?DBMS_VERSION_PATCH)
        ),
        %% TODO: OpenTelemetry support
        ?do_if(
            ServerRevision,
            ?DBMS_MIN_REVISION_WITH_OPENTELEMETRY,
            clicker_lib:encode(uint8, 0)
        ),
        ?do_if(
            ServerRevision,
            ?DBMS_MIN_REVISION_WITH_PARALLEL_REPLICAS,
            encode_parallel_replicas(Options)
        )
    ].

encode_parallel_replicas(Options) ->
    iolist_to_binary([
        clicker_lib:encode_varuint(maps:get(collaborate_with_initiator, Options, 0)),
        clicker_lib:encode_varuint(maps:get(count_participating_replicas, Options, 0)),
        clicker_lib:encode_varuint(maps:get(number_of_current_replica, Options, 0))
    ]).

encode_empty_block() ->
    iolist_to_binary([
        clicker_lib:encode_varuint(1),
        clicker_lib:encode(boolean, false),
        %clicker_lib:encode_string("is_overflows"),
        clicker_lib:encode_varuint(2),
        clicker_lib:encode(int32, -1),
        %clicker_lib:encode_string("bucket_num"),
        clicker_lib:encode_varuint(0),
        clicker_lib:encode_varuint(0),
        clicker_lib:encode_varuint(0)
    ]).

encode_data(Data) ->
    iolist_to_binary([
        clicker_lib:encode_varuint(?CLIENT_DATA),
        clicker_lib:encode_string(""),
        Data
    ]).

%% Decode query result packets
decode_query_result(Packet) ->
    decode_query_result_packets(Packet, []).

decode_query_result_packets(<<>>, Acc) ->
    {ok, lists:reverse(Acc)};
decode_query_result_packets(Packet, Acc) ->
    case clicker_lib:decode_varuint(Packet) of
        {?SERVER_DATA, Rest} ->
            case decode_data_packet(Rest) of
                {ok, DataBlock, Remaining} ->
                    decode_query_result_packets(Remaining, [{data, DataBlock} | Acc]);
                {error, Reason} ->
                    {error, Reason}
            end;
        {?SERVER_END_OF_STREAM, Rest} ->
            decode_query_result_packets(Rest, [end_of_stream | Acc]);
        {?SERVER_EXCEPTION, Rest} ->
            case decode_exception_packet(Rest) of
                {ok, Exception, Remaining} ->
                    decode_query_result_packets(Remaining, [{exception, Exception} | Acc]);
                {error, Reason} ->
                    {error, Reason}
            end;
        {?SERVER_PROGRESS, Rest} ->
            case decode_progress_packet(Rest) of
                {ok, Progress, Remaining} ->
                    decode_query_result_packets(Remaining, [{progress, Progress} | Acc]);
                {error, Reason} ->
                    {error, Reason}
            end;
        {PacketType, Rest} ->
            % For unknown packet types, just include them as raw data
            decode_query_result_packets(Rest, [{unknown, PacketType} | Acc]);
        _ ->
            {error, {invalid_packet, Packet}}
    end.

%% Decode data packet (simplified version - basic structure)
decode_data_packet(Bin) ->
    try
        % Skip temporary table name (empty string)
        {_TempTableName, Bin1} = clicker_lib:decode_string(Bin),
        % Decode block info structure
        case decode_block_info(Bin1) of
            {ok, BlockInfo, Remaining} ->
                {ok, #{block_info => BlockInfo}, Remaining};
            {error, Reason} ->
                {error, Reason}
        end
    catch
        _:Error ->
            {error, {decode_data_packet_failed, Error}}
    end.

%% Decode block info (simplified)
decode_block_info(Bin) ->
    try
        {_Field1, Bin1} = clicker_lib:decode_varuint(Bin),
        {ok, _IsOverflow, Bin2} = clicker_lib:decode(boolean, Bin1),
        {_Field2, Bin3} = clicker_lib:decode_varuint(Bin2),
        {ok, _BucketNum, Bin4} = clicker_lib:decode(int32, Bin3),
        {_Field3, Bin5} = clicker_lib:decode_varuint(Bin4),
        {NumColumns, Bin6} = clicker_lib:decode_varuint(Bin5),
        {NumRows, Remaining} = clicker_lib:decode_varuint(Bin6),
        
        BlockInfo = #{
            num_columns => NumColumns,
            num_rows => NumRows
        },
        {ok, BlockInfo, Remaining}
    catch
        _:Error ->
            {error, {decode_block_info_failed, Error}}
    end.

%% Decode exception packet
decode_exception_packet(Bin) ->
    try
        {ok, Code, Bin1} = clicker_lib:decode(int32, Bin),
        {Name, Bin2} = clicker_lib:decode_string(Bin1),
        {Message, Bin3} = clicker_lib:decode_string(Bin2),
        {StackTrace, Remaining} = clicker_lib:decode_string(Bin3),
        
        Exception = #{
            code => Code,
            name => binary_to_list(Name),
            message => binary_to_list(Message),
            stack_trace => binary_to_list(StackTrace)
        },
        {ok, Exception, Remaining}
    catch
        _:Error ->
            {error, {decode_exception_failed, Error}}
    end.

%% Decode progress packet (simplified)
decode_progress_packet(Bin) ->
    try
        {ReadRows, Bin1} = clicker_lib:decode_varuint(Bin),
        {ReadBytes, Bin2} = clicker_lib:decode_varuint(Bin1),
        {TotalRows, Remaining} = clicker_lib:decode_varuint(Bin2),
        
        Progress = #{
            read_rows => ReadRows,
            read_bytes => ReadBytes,
            total_rows => TotalRows
        },
        {ok, Progress, Remaining}
    catch
        _:Error ->
            {error, {decode_progress_failed, Error}}
    end.
