-module(clicker_protocol).

-include("clicker.hrl").

-export([
    encode_hello/1,
    encode_ping/0,
    decode_hello/1,
    decode_pong/1,
    maybe_addendum/2
]).

-define(VERSION_MAJOR, 22).
-define(VERSION_MINOR, 8).

encode_hello(Config) ->
    ClientName = maps:get(client_name, Config, "clicker"),
    VersionMajor = maps:get(version_major, Config, ?VERSION_MAJOR),
    VersionMinor = maps:get(version_minor, Config, ?VERSION_MINOR),
    ProtocolVersion = maps:get(protocol_version, Config, ?CH_SERVER_TCP_PROTOCOL_VERSION),
    Database = maps:get(database, Config, ""),
    User = maps:get(user, Config, "default"),
    Password = maps:get(password, Config, ""),
    iolist_to_binary(
        [
            clicker_lib:encode_varuint(?CH_CLIENT_HELLO),
            clicker_lib:encode_string(iolist_to_binary(["ClickHouse", " ", ClientName])),
            clicker_lib:encode_varuint(VersionMajor),
            clicker_lib:encode_varuint(VersionMinor),
            clicker_lib:encode_varuint(ProtocolVersion),
            clicker_lib:encode_string(Database),
            clicker_lib:encode_string(User),
            clicker_lib:encode_string(Password)
        ]
    ).

decode_hello(Bin0) ->
    {?CH_SERVER_HELLO, Bin1} = clicker_lib:decode_varuint(Bin0),
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

encode_ping() ->
    clicker_lib:encode_varuint(?CH_CLIENT_PING).

decode_pong(Bin) ->
    {Pong, _Tail} = clicker_lib:decode_varuint(Bin),
    Pong.

maybe_addendum(ServerRevision, Config) when
    ServerRevision >= ?CH_SERVER_MIN_PROTOCOL_VERSION_WITH_ADDENDUM
->
    {ok, clicker_lib:encode_string(maps:get(quota_key, Config, ""))};
maybe_addendum(_, _) ->
    ok.

maybe_read_server_timezone(Bin, ServerRevision) when
    ServerRevision >= ?CH_SERVER_MIN_REVISION_WITH_SERVER_TIMEZONE
->
    clicker_lib:decode_string(Bin);
maybe_read_server_timezone(Bin, _) ->
    {"", Bin}.

maybe_read_server_display_name(Bin, ServerRevision) when
    ServerRevision >= ?CH_SERVER_MIN_REVISION_WITH_DISPLAY_NAME
->
    clicker_lib:decode_string(Bin);
maybe_read_server_display_name(Bin, _) ->
    {"", Bin}.

maybe_read_server_version_patch(Bin, ServerRevision) when
    ServerRevision >= ?CH_SERVER_MIN_REVISION_WITH_VERSION_PATCH
->
    clicker_lib:decode_varuint(Bin);
maybe_read_server_version_patch(Bin, ServerRevision) ->
    {ServerRevision, Bin}.
