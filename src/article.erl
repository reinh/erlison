-module(article).

-export([download/1,parse/1]).

download(ArticleID) ->
    {ok, Article} = nntp:get_article(ArticleID),
    parse(Article).

parse(Article) when is_binary(Article) -> parse(binary_to_list(Article));
parse(Article) ->
    Lines = string:tokens(Article, "\r\n"),
    parse_lines(Lines).

parse_lines([]) -> {error, no_data};
parse_lines(["=ybegin "++Header|Lines]) -> yenc:parse(Header, Lines);
parse_lines(["begin "  ++Header|Lines]) -> uuencode:parse(Header, Lines);
parse_lines([_|Lines]) -> parse_lines(Lines). 

-include_lib("eunit/include/eunit.hrl").

parse_yenc_singlepart_test() ->
    {ok, Article} = file:read_file("../test/data/yenc/singlepart/00000005.ntx"),
    {ok, Expected} = file:read_file("../test/data/yenc/singlepart/testfile.txt"),
    {ok, Meta, Bin} = parse(binary_to_list(Article)),
    {Size, _} = string:to_integer(proplists:get_value("size", Meta)),
    ?assertEqual(Bin, Expected),
    ?assertEqual(Meta,
                 [{"name", "testfile.txt"},{"line", "128"},{"size","584"}]),
    ?assertEqual(byte_size(Bin), Size).

parse_yenc_multipart_test() ->
    {ok, Article1} = file:read_file("../test/data/yenc/multipart/00000020.ntx"),
    {ok, Article2} = file:read_file("../test/data/yenc/multipart/00000021.ntx"),
    {ok, Expected} = file:read_file("../test/data/yenc/multipart/joystick.jpg"),
    {ok, _, Bin1} = parse(binary_to_list(Article1)),
    {ok, _, Bin2} = parse(binary_to_list(Article2)),
    Bin = list_to_binary([Bin1, Bin2]),
    ?assertEqual(Bin, Expected).
