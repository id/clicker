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
    encode_data/1
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
