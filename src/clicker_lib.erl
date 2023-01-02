-module(clicker_lib).

%% API
-export([
    encode/2,
    encode_varuint/1,
    encode_string/1,
    decode/2,
    decode_varuint/1,
    decode_string/1
]).

encode(uint8, I) when is_integer(I) -> <<I:8/little-unsigned-integer>>;
encode(uint16, I) when is_integer(I) -> <<I:16/little-unsigned-integer>>;
encode(uint32, I) when is_integer(I) -> <<I:32/little-unsigned-integer>>;
encode(uint64, I) when is_integer(I) -> <<I:64/little-unsigned-integer>>;
encode(int8, I) when is_integer(I) -> <<I:8/little-signed-integer>>;
encode(int16, I) when is_integer(I) -> <<I:16/little-signed-integer>>;
encode(int32, I) when is_integer(I) -> <<I:32/little-signed-integer>>;
encode(int64, I) when is_integer(I) -> <<I:64/little-signed-integer>>;
encode(float32, F) when is_float(F) -> <<F:32/little-signed-float>>;
encode(float64, F) when is_float(F) -> <<F:64/little-signed-float>>;
encode(boolean, true) ->
    <<1:8/little-unsigned-integer>>;
encode(boolean, false) ->
    <<0:8/little-unsigned-integer>>;
encode({list, Type}, List) when is_list(List) ->
    Elements = lists:map(fun(Elem) -> encode(Type, Elem) end, List),
    [encode_varuint(length(List)), Elements];
encode({nullable, _}, undefined) ->
    encode(uint8, 1);
encode({nullable, T}, Val) ->
    [encode(uint8, 0), encode(T, Val)].

encode_varuint(0) ->
    <<0>>;
encode_varuint(U) ->
    encode_varuint(U, <<>>).

encode_varuint(0, Acc) ->
    Acc;
encode_varuint(U, Acc) when U > 127 ->
    encode_varuint(U bsr 7, <<Acc/binary, (U bor 128)/little-unsigned-integer>>);
encode_varuint(U, Acc) ->
    encode_varuint(U bsr 7, <<Acc/binary, U>>).

encode_string(S) when is_bitstring(S) ->
    [encode_varuint(byte_size(S)), S];
encode_string(S) when is_list(S) ->
    encode_string(iolist_to_binary(S)).

decode({nullable, _}, <<1, Tail/binary>>) ->
    {ok, undefined, Tail};
decode({nullable, Type}, <<0, Tail/binary>>) ->
    decode(Type, Tail);
decode({nullable, _Type} = T, <<>>) ->
    {resume, fun(Bin) -> decode(T, Bin) end};
decode({list, Type}, Bin) ->
    case decode_varuint(Bin) of
        {Len, Tail} ->
            decode_list(Type, Tail, Len, []);
        _ ->
            Decoder = fun(More) -> decode({list, Type}, <<Bin/binary, More/binary>>) end,
            {resume, Decoder}
    end;
decode(int64, <<I:64/little-signed-integer, Tail/binary>>) ->
    {ok, I, Tail};
decode(int32, <<I:32/little-signed-integer, Tail/binary>>) ->
    {ok, I, Tail};
decode(int16, <<I:16/little-signed-integer, Tail/binary>>) ->
    {ok, I, Tail};
decode(int8, <<I:8/little-signed-integer, Tail/binary>>) ->
    {ok, I, Tail};
decode(uint64, <<I:64/little-unsigned-integer, Tail/binary>>) ->
    {ok, I, Tail};
decode(uint32, <<I:32/little-unsigned-integer, Tail/binary>>) ->
    {ok, I, Tail};
decode(uint16, <<I:16/little-unsigned-integer, Tail/binary>>) ->
    {ok, I, Tail};
decode(uint8, <<I:8/little-unsigned-integer, Tail/binary>>) ->
    {ok, I, Tail};
decode(boolean, <<1:8, Tail/binary>>) ->
    {ok, true, Tail};
decode(boolean, <<0:8, Tail/binary>>) ->
    {ok, false, Tail};
decode(float64, <<F:64/little-signed-float, Tail/binary>>) ->
    {ok, F, Tail};
decode(float32, <<F:32/little-signed-float, Tail/binary>>) ->
    {ok, F, Tail};
decode(Type, Bin) ->
    {resume, fun(More) -> decode(Type, <<Bin/binary, More/binary>>) end}.

decode_list(_, Bin, 0, Acc) ->
    {ok, lists:reverse(Acc), Bin};
decode_list(Type, Bin, Count, Acc) ->
    case decode(Type, Bin) of
        {ok, Val, Tail} ->
            decode_list(Type, Tail, Count - 1, [Val | Acc]);
        {resume, _} ->
            {resume, fun(More) -> decode_list(Type, <<Bin/binary, More/binary>>, Count, Acc) end}
    end.

decode_varuint(Bin) ->
    decode_varuint(Bin, 0, 0).

decode_varuint(<<Bin:8, Tail/binary>>, Acc0, Shift) ->
    Acc = ((Bin band 127) bsl Shift) bor Acc0,
    case Bin band 128 of
        0 -> {Acc, Tail};
        _ -> decode_varuint(Tail, Acc, Shift + 7)
    end;
decode_varuint(Bin, Acc, _Shift) ->
    {Acc, Bin}.

decode_string(Bin) ->
    {Len, Tail} = decode_varuint(Bin),
    copy_bytes(Len, Tail).

copy_bytes(Len, Bin) when Len =< 0 ->
    {<<>>, Bin};
copy_bytes(Len, Bin) ->
    <<Bytes:Len/binary, Rest/binary>> = Bin,
    {binary:copy(Bytes), Rest}.
