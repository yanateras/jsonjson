%% Minimal JSON parsing module that both encodes and decodes.
%% When encoding, JSON types are mapped to Erlang terms as follows:
%%
%% * JSON object is Erlang map
%% * JSON string is Erlang binary
%% * JSON list is Erlang list
%% * JSON numbers are Erlang numbers
%%
%% When decoding, everything is vice versa except strings. Strings are
%% parsed to lists. You can alter this behavior to fit your preferences
%% by editing decode_string/2 in this file.

-module(json).

-export([encode/1, decode/1]).
-define(is_digit(X), X >= 48, X =< 57).
-define(is_space(X), X =< 32; X == $,).
-define(is_exponent(X), X == $e; X == $E).
-define(is_sign(X), X == $+; X == $-).

encode(Bin) when is_binary(Bin) -> encode_string(Bin, <<$">>);
encode(I) when is_integer(I) -> integer_to_binary(I);
encode(F) when is_float(F) -> io_lib:format("~p", [F]);
encode(M) when is_map(M) -> encode_map(maps:to_list(M), []);
encode([{_, _}, _] = Props) -> encode_map(Props, []);
encode(L) when is_list(L) -> encode_list(L, []);
encode(true) -> <<"true">>;
encode(false) -> <<"false">>;
encode(null) -> <<"null">>;
encode(A) when is_atom(A) -> encode(list_to_binary(atom_to_list(A))).

encode_string(<<>>, Buf) -> <<Buf/binary, $">>;
encode_string(<<$\r, T/binary>>, Buf) -> encode_string(T, <<Buf/binary, $\\, $r>>);
encode_string(<<$\t, T/binary>>, Buf) -> encode_string(T, <<Buf/binary, $\\, $t>>);
encode_string(<<$\n, T/binary>>, Buf) -> encode_string(T, <<Buf/binary, $\\, $n>>);
encode_string(<<$\f, T/binary>>, Buf) -> encode_string(T, <<Buf/binary, $\\, $f>>);
encode_string(<<$\\, T/binary>>, Buf) -> encode_string(T, <<Buf/binary, $\\, $\\>>);
encode_string(<<$", T/binary>>, Buf) -> encode_string(T, <<Buf/binary, $\\, $">>);
encode_string(<<H, T/binary>>, Buf) -> encode_string(T, <<Buf/binary, H>>).

encode_list([], Buf) -> [$[, lists:reverse(Buf), $]];
encode_list([H], Buf) -> encode_list([], [encode(H) | Buf]); 
encode_list([H|T], Buf) -> encode_list(T, [$, | [encode(H) | Buf]]).

encode_map([], Buf) -> [${, lists:reverse(Buf), $}];
encode_map([H], Buf) -> encode_map([], [encode_pair(H) | Buf]); 
encode_map([H|T], Buf) -> encode_map(T, [$, | [encode_pair(H) | Buf]]).

encode_pair({K,V}) -> [encode(K), $:, encode(V)].

decode(String) when is_list(String) -> decode(list_to_binary(String));
decode(Bin) when is_binary(Bin) ->
    try decode_value(Bin) of
        {Rest, Value} -> {ok, Value, Rest}
    catch error:Reason -> {error, Reason} 
    end;
decode(_) -> error(badarg).

decode_value(<<$", T/binary>>) -> decode_string(T, <<>>);
decode_value(<<$[, T/binary>>) -> decode_array(T, []);
decode_value(<<${, T/binary>>) -> decode_object(T, #{});

decode_value(<<H, T/binary>>) when ?is_digit(H); ?is_sign(H) -> decode_number(T, [H]);
decode_value(<<H, T/binary>>) when ?is_space(H) -> decode_value(T);

decode_value(<<"true", T/binary>>) -> {T, true};
decode_value(<<"false", T/binary>>) -> {T, false};
decode_value(<<"null", T/binary>>) -> {T, null};

decode_value(_) -> error(unexpected_token).

decode_number(Bin, Buf) ->
    {Rest, Acc} = decode_sign(Bin, Buf), 
    decode_integer(Rest, Acc).

decode_integer(<<H, T/binary>>, Buf) when ?is_digit(H) -> decode_integer(T, [H|Buf]);
decode_integer(<<$., T/binary>>, Buf) -> decode_fraction(T, [$.|Buf]);
decode_integer(<<H, T/binary>>, Buf) when ?is_exponent(H) ->
    {Rest1, Acc} = decode_sign(T, [H, $0, $.] ++ Buf),
    decode_exponent(Rest1, Acc);
decode_integer(_, [H|_]) when ?is_sign(H) -> error(unterminated_integer);
decode_integer(Bin, Buf) -> {Bin, list_to_integer(lists:reverse(Buf))}.

decode_fraction(<<H, T/binary>>, Buf) when ?is_digit(H) -> decode_fraction(T, [H|Buf]);
decode_fraction(<<H, T/binary>>, Buf) when ?is_exponent(H) ->
    {Rest, Acc} = decode_sign(T, [H|Buf]),
    decode_exponent(Rest, Acc);
decode_fraction(_, [H|_]) when H == $.; ?is_sign(H) -> error(unterminated_fraction);
decode_fraction(Bin, Buf) -> {Bin, list_to_float(lists:reverse(Buf))}.

decode_exponent(<<H, T/binary>>, Buf) when ?is_digit(H) -> decode_exponent(T, [H|Buf]);
decode_exponent(_, [H|_]) when ?is_exponent(H); ?is_sign(H) -> error(unterminated_exponent);
decode_exponent(Bin, Buf) ->
    try list_to_float(lists:reverse(Buf)) of
        Float -> {Bin, Float}
    catch error:badarg -> error(infinity)
    end.

decode_sign(<<H, T/binary>>, Buf) when ?is_sign(H) -> {T, [H|Buf]};
decode_sign(Bin, Buf) -> {Bin, Buf}.

decode_string(<<$", T/binary>>, Buf) -> {T, Buf};
decode_string(<<$\\, $u, C1, C2, C3, C4, T/binary>>, Buf) ->
    try C = list_to_integer([C1, C2, C3, C4], 16),
        if C > 16#D7FF, C < 16#DC00 ->
            <<$\\, $u, D1, D2, D3, D4, T2/binary>> = T,
            D = list_to_integer([D1, D2, D3, D4], 16),
            Point = xmerl_ucs:from_utf16be(<<C:16/big-unsigned-integer, D:16/big-unsigned-integer>>),
            Char = unicode:characters_to_binary(Point),
            decode_string(T2, <<Buf/binary, Char/binary>>);
        true ->
            Char = unicode:characters_to_binary([C]),
            decode_string(T, <<Buf/binary, Char/binary>>)
        end
    catch error:badarg -> error(invalid_escape)
    end;
decode_string(<<$\\, $b, T/binary>>, Buf) -> decode_string(T, <<Buf/binary, $\s>>);
decode_string(<<$\\, $f, T/binary>>, Buf) -> decode_string(T, <<Buf/binary, $\f>>);
decode_string(<<$\\, $n, T/binary>>, Buf) -> decode_string(T, <<Buf/binary, $\n>>);
decode_string(<<$\\, $r, T/binary>>, Buf) -> decode_string(T, <<Buf/binary, $\r>>);
decode_string(<<$\\, $t, T/binary>>, Buf) -> decode_string(T, <<Buf/binary, $\t>>);
decode_string(<<$\\,  H, T/binary>>, Buf) -> decode_string(T, <<Buf/binary, H>>);
decode_string(<<H, T/binary>>, Buf) -> decode_string(T, <<Buf/binary, H>>);
decode_string(<<>>, _) -> error(unterminated_string).

decode_array(<<H, T/binary>>, List) when ?is_space(H) -> decode_array(T, List);
decode_array(<<$], T/binary>>, List) -> {T, lists:reverse(List)};
decode_array(<<>>, _) -> error(unterminated_array);
decode_array(Bin, List) ->
    {Rest, Value} = decode_value(Bin),
    decode_array(Rest, [Value|List]).

decode_object(<<H, T/binary>>, Map) when ?is_space(H) -> decode_object(T, Map);
decode_object(<<$}, T/binary>>, Map) -> {T, Map};
decode_object(<<>>, _) -> error(unterminated_object);
decode_object(Bin, Map) ->
    {Rest1, Key} = decode_value(Bin),
    {Rest2, $:} = decode_colon(Rest1),
    {Rest3, Value} = decode_value(Rest2),
    decode_object(Rest3, maps:put(Key, Value, Map)).

decode_colon(<<$:, T/binary>>) -> {T, $:};
decode_colon(<<H, T/binary>>) when ?is_space(H) -> decode_colon(T);
decode_colon(_) -> error(missing_colon).
