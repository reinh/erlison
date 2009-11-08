-module (yenc).

-export ([parse_data/2,decode/1]).

parse_data(Header, Lines) ->
    Meta = parse_header(Header),
    Data = parse_body(Lines),
    Bin  = decode(Data),
    {ok, Meta, Bin}.

parse_header(Header) ->
    [ list_to_tuple(string:tokens(X, "=")) || X<-string:tokens(Header, " ")].

%% collect data body up to ending =yend line
parse_body(Lines) ->
    parse_body(Lines, []).
parse_body(["=yend"++_|_], Acc) -> lists:reverse(Acc);
parse_body([Line|Rest], Acc) ->
    parse_body(Rest, [Line|Acc]).
    
decode(List) when is_list(List) -> decode(list_to_binary(List));
decode(<<$=, X, Rest/binary>>)  -> <<(X-106):8, (decode(Rest))/binary>>;
decode(<<X, Rest/binary>>)      -> <<(X-42):8, (decode(Rest))/binary>>;
decode(<<>>)                    -> <<>>.

-include_lib("eunit/include/eunit.hrl").

decode_test_() ->
    [?_assertEqual(<<"Hello World">>, decode(<<114,143,150,150,153,74,129,153,156,150,142>>)),
     ?_assertEqual(<<19>>,            decode(<<61, 125>>))].

parse_data_bin_test() ->
    ?assertEqual({ok, [], <<"Hello World">>}, parse_data("", [[114,143,150,150,153,74,129,153,156,150,142], "=yend"])).

parse_data_header_test_() ->
    [?_assertEqual({ok, [{"foo", "bar"}], <<>>}, parse_data("foo=bar", [[],"=yend"])),
     ?_assertEqual({ok, [{"foo", "bar"}, {"file", "foobar.txt"}], <<>>}, parse_data("foo=bar file=foobar.txt", [[],"=yend"]))].
