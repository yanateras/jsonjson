json
====

Simple, pure, optimized JSON module that both encodes and decodes.
Use it when you need a clean, pure Erlang module without dependencies that just works
and can be easily audited. Use [Jiffy][] if you seek performance.

Encoding
--------

To encode Erlang term to JSON, use `json:encode/1`. Mind that it returns
an `iolist`, but any Erlang IO supports them just like a binary. If you
want to flatten it, use `iolist_to_binary`:

```
Eshell V6.4  (abort with ^G)
1> Value = json:encode(#{name => <<"Jim">>, age => 25, children => [<<"Nick">>, <<"Travis">>]}).
[123,
 [<<"\"age\"">>,58,<<"25">>],
 44,
 [<<"\"children\"">>,58,
  [91,<<"\"Nick\"">>,44,<<"\"Travis\"">>,93]],
 44,
 [<<"\"name\"">>,58,<<"\"Jim\"">>],
 125]
2> iolist_to_binary(Value).
<<"{\"age\":25,\"children\":[\"Nick\",\"Travis\"],\"name\":\"Jim\"}">>
```

When encoding, Erlang terms are mapped to JSON types as follows:

Erlang term | JSON type
------------|----------
map         | object
binary      | string
list        | array
integer     | integer
float       | float
true        | true
false       | false
null        | null
atom        | string

Here is a comparison of JSON encoding between Erlang implementations:

Implementations | Microseconds | Baseline speed | Lines of code              | Dependencies      | Pure?
----------------|--------------|----------------|----------------------------|-------------------|------
[jiffy][]       | 272805       | 2.929x         | 166 Erlang + 2449 C        | double-conversion | ✗
**json**        | **799019**   | **1x**         | **115**                    | **✗**             | **✓**
[json2][]       | 1613420      | 0.495x         | 542                        | ✗                 | ✓
[json_eep][]    | 3628160      | 0.22x          | 349                        | leex              | ✓

Decoding
--------

To decode binary string to Erlang term, use `json:decode/1`. It takes a binary, and note that while
it can take a list, it will call apply `list_to_binary/1` to it and process it as a binary anyway.
You can find list-powered decoder [in one of the earlier revisions][decode-list]. If you have a use-case for
list decoding (as opposed to binary decoding), please tell me by opening an issue: then I will optimize it.

[decode-list]: https://github.com/yegortimoschenko/json/blob/3bdc3e7b7bb285ca3405bdcd86bb203e8eb93a1f/src/json.erl

Here is an example: 

```
Eshell V6.4  (abort with ^G)
1> json:decode(<<"{\"name\": \"The Dark\",\"code\": \"DRK\",\"releaseDate\": \"1994-08-01\",\"is_rare\": false}">>).
{ok,#{"code" => "DRK",
      "is_rare" => false,
      "name" => "The Dark",
      "releaseDate" => "1994-08-01"}}
```

When decoding, everything is the same **except strings**. JSON strings are parsed to Erlang lists.
It is to save `list_to_binary/1` call.

Mind that key order in JSON objects is not preserved. From [RFC 7159](http://www.rfc-editor.org/rfc/rfc7159.txt):

> An object is an unordered collection of zero or more name/value pairs,
> where a name is a string and a value is a string, number, boolean, null, object, or array.

Here is a comparison of JSON decoding between Erlang implementations:

Implementations | Microseconds | Baseline speed | Lines of code              | Dependencies      | Pure?
----------------|--------------|----------------|----------------------------|-------------------|------
[jiffy][]       | 145011       | 5.99x          | 166 Erlang + 2449 C        | double-conversion | ✗
**json**        | **869037**   | **1x**         | **115**                    | **✗**             | **✓**
[json2][]       | 1528941      | 0.57x          | 542                        | ✗                 | ✓
[json_eep][]    | 3594663      | 0.24x          | 349                        | leex              | ✓

[Jiffy]: https://github.com/davisp/jiffy
[json2]: https://github.com/klacke/yaws/blob/master/src/json2.erl
[json_eep]: https://github.com/jchris/erlang-json-eep-parser

Benchmarks
----------

Benchmarks are done on [MTG JSON v2.19.2](http://mtgjson.com) `AllSets.json` file
using `timer:tc/3` function. Each implementation is tested 50 times, then the lowest time is used.
The host is MacBook Air 13″ Mid 2013. The operating system is OS X. Erlang settings are as follows:

```
Erlang/OTP 17 [erts-6.4] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false] [dtrace]
```

If implementation supports only lists on decoding, time spent on `binary_to_list/1` is included.
