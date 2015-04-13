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
-define(is_space(X), X == $\t; X == $\s; X == $\t; X == $\n).

encode(Bin) when is_binary(Bin) ->
    EscapedBin = escape(Bin), 
    <<"\"", EscapedBin/binary, "\"">>; 
encode(I) when is_integer(I) -> integer_to_binary(I);
encode(F) when is_float(F) -> float_to_binary(F);
encode(M) when is_map(M) -> encode_map(maps:to_list(M), []);
encode(L) when is_list(L) -> encode_list(L, []);
encode(true) -> <<"true">>;
encode(false) -> <<"false">>;
encode(null) -> <<"null">>;
encode(A) when is_atom(A) -> encode(list_to_binary(atom_to_list(A))).

escape(Bin) ->
    Substitutions = [
        {<<"\\">>, <<"\\\\">>}, {<<"\"">>, <<"\\\"">>},
        {<<"\f">>, <<"\\f">>}, {<<"\n">>, <<"\\n">>},  
        {<<"\r">>, <<"\\r">>}, {<<"\t">>, <<"\\t">>}
    ],
    lists:foldl(fun({K,V},Acc) -> binary:replace(Acc, K, V) end, Bin, Substitutions).

%% Wraps a list of binaries with First and Last elements and concatenates them all.
wrap(List, First, Last) -> iolist_to_binary([First | lists:reverse([Last | List])]).

add_comma(Bin) -> <<Bin/binary, ",">>.

encode_list([], Acc) -> wrap(Acc, <<"[">>, <<"]">>);
encode_list([H], Acc) -> encode_list([], [encode(H) | Acc]); 
encode_list([H|T], Acc) -> encode_list(T, [add_comma(encode(H)) | Acc]).

encode_map([], Acc) -> wrap(Acc, <<"{">>, <<"}">>);
encode_map([H], Acc) -> encode_map([], [encode_pair(H) | Acc]); 
encode_map([H|T], Acc) -> encode_map(T, [add_comma(encode_pair(H)) | Acc]).

encode_pair({K,V}) -> 
    Key = encode(K), Value = encode(V), 
    <<Key/binary, ":", Value/binary>>.

decode(Bin) when is_binary(Bin) -> decode(binary_to_list(Bin));
decode(String) -> {_, Value} = decode(String, []), Value.

decode([$"|T], []) -> decode_string(T, []);
decode([$[|T], []) -> decode_list(T, []);
decode([${|T], []) -> decode_map(T, #{});

decode([H|T], []) when ?is_space(H) -> decode(T, []);

decode([$t|T], []) -> decode(T, "t");
decode([$r|T], "t") -> decode(T, "tr");
decode([$u|T], "tr") -> decode(T, "tru");
decode([$e|T], "tru") -> {T, true};

decode([$f|T], []) -> decode(T, "f");
decode([$a|T], "f") -> decode(T, "fa");
decode([$l|T], "fa") -> decode(T, "fal");
decode([$s|T], "fal") -> decode(T, "fals");
decode([$e|T], "fals") -> {T, false};

decode([$n|T], []) -> decode(T, "n");
decode([$u|T], "n") -> decode(T, "nu");
decode([$l|T], "nu") -> decode(T, "nul");
decode([$l|T], "nul") -> {T, null};

decode(Chars=[H|_], []) when ?is_digit(H) -> decode_integer(Chars, []).

decode_integer([H|T], Buf) when ?is_digit(H) -> decode_integer(T, [H|Buf]);
decode_integer([$.|T], Buf) -> decode_float(T, [$.|Buf]);
decode_integer(Chars, Buf) -> {Chars, list_to_integer(lists:reverse(Buf))}.

decode_float([H|T], Buf) when ?is_digit(H); H == $e; H == $E; H == $+ ->
    decode_float(T, [H|Buf]);
decode_float(Chars, Buf) -> {Chars, list_to_float(lists:reverse(Buf))}.

decode_string([$\\|Chars], Buf) ->
    Char = hd(Chars),
    EscapeCode = case Char of
        $b -> $\s;
        $f -> $\f;
        $n -> $\n;
        $r -> $\r;
        $t -> $\t;
        Char -> Char
    end,
    decode_string(tl(Chars), [EscapeCode|Buf]);
decode_string([$"|Chars], Buf) -> {Chars, lists:reverse(Buf)};
decode_string([H|T], Buf) -> decode_string(T, [H|Buf]).

decode_list([$] |Chars], List) -> {Chars, lists:reverse(List)};
decode_list([$, |Chars], List) -> decode_list(Chars, List);
decode_list([H|T], List) when ?is_space(H) -> decode_list(T, List);
decode_list(Chars, List) ->
    {Rest, Value} = decode(Chars, []),
    decode_list(Rest, [Value|List]).

decode_map([$}|Chars], Map) -> {Chars, Map};
decode_map([$,|Chars], Map) -> decode_map(Chars, Map);
decode_map([H|T], Map) when ?is_space(H) -> decode_map(T, Map);
decode_map(Chars, Map) ->
    {Rest1, Key} = decode(Chars, []),
    {Rest2, ok} = decode_colon(Rest1),
    {Rest3, Value} = decode(Rest2, []),
    decode_map(Rest3, maps:put(Key, Value, Map)).

decode_colon([$:|T]) -> {T, ok};
decode_colon([H|T]) when ?is_space(H) -> decode_colon(T).
