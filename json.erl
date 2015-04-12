%% Minimal JSON parsing module that both encodes and decodes.
%% JSON types are mapped to Erlang terms as follows:
%%
%% * JSON object is Erlang map
%% * JSON string is Erlang binary
%% * JSON list is Erlang list
%% * JSON numbers are Erlang numbers, but exponent is not supported
-module(json).

-export([encode/1, decode/1]).
-define(is_digit(X), X >= 48, X =< 57).
-define(is_space(X), X == $\t, X == $\s, X == $\t, X == $\n).

encode(Bin) when is_binary(Bin) -> <<"\"", Bin/binary, "\"">>; 
encode(I) when is_integer(I) -> integer_to_binary(I);
encode(M) when is_map(M) -> encode_map(maps:to_list(M), []);
encode(L) when is_list(L) -> encode_list(L, []);
encode(true) -> <<"true">>;
encode(false) -> <<"false">>;
encode(null) -> <<"null">>;
encode(A) when is_atom(A) -> encode(list_to_binary(atom_to_list(A))).

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

decode(String) -> {Value, _} = decode(String, []), Value.

decode(Bin, Buf) when is_binary(Bin) -> decode(binary_to_list(Bin), Buf);

decode([$"|T], []) -> decode_string(T, []);
decode([$[|T], []) ->
    [Rest|List] = lists:reverse(decode_list(T)),
    {lists:reverse(List), Rest};

decode([${|T], []) -> decode_map(T, #{});
decode([H|T], []) when ?is_space(H) -> decode(T, []);

decode([$t|T], []) -> decode(T, "t");
decode([$r|T], "t") -> decode(T, "tr");
decode([$u|T], "tr") -> decode(T, "tru");
decode([$e|T], "tru") -> {true, T};

decode([$f|T], []) -> decode(T, "f");
decode([$a|T], "f") -> decode(T, "fa");
decode([$l|T], "fa") -> decode(T, "fal");
decode([$s|T], "fal") -> decode(T, "fals");
decode([$e|T], "fals") -> {false, T};

decode([$n|T], []) -> decode(T, "n");
decode([$u|T], "n") -> decode(T, "nu");
decode([$l|T], "nu") -> decode(T, "nul");
decode([$l|T], "nul") -> {null, T};

decode(Chars=[H|_], []) when ?is_digit(H) -> decode_integer(Chars, []).

decode_integer([H|T], Buf) when ?is_digit(H) -> decode_integer(T, [H|Buf]);
decode_integer([$.|T], Buf) -> decode_float(T, [$.|Buf]);
decode_integer(Chars, Buf) -> {list_to_integer(lists:reverse(Buf)), Chars}.

decode_float([H|T], Buf) when ?is_digit(H) -> decode_float(T, [H|Buf]);
decode_float(Chars, Buf) -> {list_to_float(lists:reverse(Buf)), Chars}.

decode_string([$"|Chars], Buf) -> {lists:reverse(Buf), Chars};
decode_string([H|T], Buf) -> decode_string(T, [H|Buf]).

decode_list([$] |Chars]) -> [Chars];
decode_list([$, |Chars]) -> decode_list(Chars);
decode_list([H|T]) when ?is_space(H) -> decode_list(T);
decode_list(Chars) ->
    {Value, Rest} = decode(Chars, []),
    [Value|decode_list(Rest)].

decode_map([$}|Chars], Map) -> {Map, Chars};
decode_map([$,|Chars], Map) -> decode_map(Chars, Map);
decode_map([H|T], Map) when ?is_space(H) -> decode_map(T, Map);
decode_map(Chars, Map) ->
    {Key, Rest} = decode(Chars, []),
    {ok, Rest2} = decode_colon(Rest),
    {Value, Rest3} = decode(Rest2, []),
    decode_map(Rest3, maps:put(Key, Value, Map)).

decode_colon([$:|T]) -> {ok, T};
decode_colon([H|T]) when ?is_space(H) -> decode_colon(T).
