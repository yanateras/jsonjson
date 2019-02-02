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

-spec encode(Term) -> JSON when Term :: term(), JSON :: iodata().
%% @doc Encode Term as a JSON value.
%%
%% <table>
%%   <tr>
%%     <th>Term</th>
%%     <th>JSON</th>
%%   </tr>
%%   <tr>
%%     <td>{@type false}</td>
%%     <td>false</td>
%%   </tr>
%%   <tr>
%%     <td>{@type null}</td>
%%     <td>null</td>
%%   </tr>
%%   <tr>
%%     <td>{@type true}</td>
%%     <td>true</td>
%%   </tr>
%%   <tr>
%%     <td>{@type binary()}</td>
%%     <td>string</td>
%%   </tr>
%%   <tr>
%%     <td>{@type map()}</td>
%%     <td>object</td>
%%   </tr>
%%   <tr>
%%     <td>{@type list()}</td>
%%     <td>array</td>
%%   </tr>
%%   <tr>
%%     <td>{@type number()}</td>
%%     <td>number</td>
%%   </tr>
%% </table>
%%
%% Note that {@type string()} is treated as an array of numbers:
%% ```
%% <<"[106,111,101]">> = iolist_to_binary(json:encode("joe")).
%% '''
%%
%% Throws {@type badarg} if map or tuple key does not encode to a JSON string.
encode(Bin) when is_binary(Bin) -> encode_string(Bin, <<$">>);
encode(I) when is_integer(I) -> integer_to_binary(I);
encode(F) when is_float(F) -> io_lib:format("~p", [F]);
encode(L) when is_list(L) -> encode_list(L, []);
encode(M) when is_map(M) -> encode_map(maps:to_list(M), []);
encode(true) -> <<"true">>;
encode(false) -> <<"false">>;
encode(null) -> <<"null">>.

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
encode_map([H], Buf) -> encode_map([], [encode_map_pair(H) | Buf]);
encode_map([H|T], Buf) -> encode_map(T, [$, | [encode_map_pair(H) | Buf]]).

encode_map_pair({K,V}) when is_binary(K) -> [encode(K), $:, encode(V)];
encode_map_pair(_) -> error(badarg).

-spec decode(JSON) -> {ok, Term, Rest} | {error, Reason}
			  when JSON :: iodata(),
			       Term :: term(),
			       Rest :: binary(),
			       Reason :: infinity
				       | invalid_escape
				       | invalid_key
				       | missing_colon
				       | unterminated_array
				       | unterminated_exponent
				       | unterminated_fraction
				       | unterminated_integer
				       | unterminated_object
				       | unterminated_string.
%% @doc Decode JSON value to Term. Unconsumed characters are returned as Rest.
%%
%% <table>
%%   <tr>
%%     <th>JSON</th>
%%     <th>Term</th>
%%   </tr>
%%   <tr>
%%     <td>array</td>
%%     <td>{@type list()}</td>
%%   </tr>
%%   <tr>
%%     <td>false</td>
%%     <td>{@type false}</td>
%%   </tr>
%%   <tr>
%%     <td>number</td>
%%     <td>{@type number()}</td>
%%   </tr>
%%   <tr>
%%     <td>null</td>
%%     <td>{@type null}</td>
%%   </tr>
%%   <tr>
%%     <td>object</td>
%%     <td>{@type map()}</td>
%%   </tr>
%%   <tr>
%%     <td>string</td>
%%     <td>{@type binary()}</td>
%%   </tr>
%%   <tr>
%%     <td>true</td>
%%     <td>{@type true}</td>
%%   </tr>
%% </table>
%%
%% Decoding is more lenient than the standard in the following:
%% <ul>
%%   <li>any character less than or equal to 32 on ASCII table is treated as whitespace</li>
%%   <li>
%%     commas are treated as whitespace:
%% ```
%% {ok, [1,2,3,4], _} = json:decode(<<",[,,,1  2,3, ,4]">>).
%% {ok, [], _} = json:decode(<<"[,, ,,]">>).
%% '''
%%    </li>
%%    <li>
%%      whitespace is optional on token boundaries:
%% ```
%% {ok, [<<"hello">>, true, 1, null], _} = json:decode(<<"[\"hello\"true1null]">>).
%% '''
%%    </li>
%%    <li>
%%      numbers may contain leading zeros:
%% ```
%% {ok, 4, _} = json:decode(<<"0004">>).
%% {ok, 1.0, _} = json:decode(<<"1e-0000">>).
%% '''
%%    </li>
%%    <li>
%%      numbers may be prefixed with a plus sign:
%% ```
%% {ok, 100, _} = json:decode(<<"+100">>).
%% '''
%%    </li>
%% </ul>
%%
%% Does not handle JSON that contains numbers with a fraction part and/or
%% exponent larger than 1.8e308 (IEE 754-1985 double precision):
%% ```
%% {error, infinity} = json:decode(<<"1e1000">>).
%% '''
%%
%% Does <em>not</em> preserve key order in objects, as per RFC 7159.
%%
%% Throws {@type badarg} if JSON is not of type {@type iodata()}.
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
decode_object(<<$", Bin/binary>>, Map) ->
    {Rest1, Key} = decode_string(Bin, <<>>),
    {Rest2, $:} = decode_colon(Rest1),
    {Rest3, Value} = decode_value(Rest2),
    decode_object(Rest3, maps:put(Key, Value, Map));
decode_object(_, _) -> error(invalid_key).

decode_colon(<<$:, T/binary>>) -> {T, $:};
decode_colon(<<H, T/binary>>) when ?is_space(H) -> decode_colon(T);
decode_colon(_) -> error(missing_colon).
