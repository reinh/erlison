-module(article).

-export([parse/1]).

parse(Article) ->
    Lines = string:tokens(Article, "\r\n"),
    parse_lines(Lines).

parse_lines([]) -> {error, no_data};
parse_lines(["=ybegin "++Header|Lines]) -> yenc:parse_data(Header, Lines);
parse_lines(["begin "  ++Header|Lines]) -> uuencode:parse_data(Header, Lines);
parse_lines([_|Lines]) -> parse_lines(Lines). 

-include_lib("eunit/include/eunit.hrl").

parse_yenc_singlepart_test() ->
    {ok, Article} = file:read_file("../test/data/yenc/singlepart/00000005.ntx"),
    {ok, Expected} = file:read_file("../test/data/yenc/singlepart/testfile.txt"),
    {ok, Meta, Bin} = parse(binary_to_list(Article)),
    {Size, _} = string:to_integer(proplists:get_value("size", Meta)),
    ?assertEqual(Bin, Expected),
    ?assertEqual(Meta, [{"line", "128"}, {"size", "584"}, {"name", "testfile.txt"}]),
    ?assertEqual(byte_size(Bin), Size).
