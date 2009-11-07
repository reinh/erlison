-module(article).

-export([parse/1]).

parse(Article) ->
    Lines = string:tokens(Article, "\r\n"),
    parse_article_lines(Lines).

parse_article_lines([Line|Lines]) ->
    parse_article_lines(Line, Lines).
parse_article_lines([$=, $y, $b, $e, $g, $i, $n, $\s | MetaLine], Rest) ->
    parse_data(yenc, MetaLine, Rest);
parse_article_lines([$b, $e, $g, $i, $n, $\s | MetaLine], Rest) ->
    parse_data(uue, MetaLine, Rest);
parse_article_lines(_, []) ->
    {error, no_data}.

