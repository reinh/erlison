-module (yenc).

-export ([parse_data/2,dec/1]).

parse_data(Header, Lines) ->
    Meta = parse_header(Header),
    Data = parse_body(Lines),
    Bin  = dec(Data),
    {ok, Meta, Bin}.

parse_header(Header) -> Header.

%% collect data body up to ending =yend line
parse_body(Lines) ->
    parse_body(Lines, []).
parse_body([[$=, $y, $e, $n, $d | _] | _], Acc) -> lists:reverse(Acc);
parse_body([Line|Rest], Acc) ->
    parse_body(Rest, [Line|Acc]).
    

dec(List) when is_list(List) -> dec(list_to_binary(List));
dec(<<$=, X, Rest/binary>>)  -> <<(X-106 rem 256):8, (dec(Rest))/binary>>;
dec(<<X, Rest/binary>>)      -> <<(X-42 rem 256):8, (dec(Rest))/binary>>;
dec(<<>>)                    -> <<>>.

-include_lib("eunit/include/eunit.hrl").

dec_test_() ->
    [?_assertEqual(<<"Hello World">>, dec(<<114,143,150,150,153,74,129,153,156,150,142>>)),
     ?_assertEqual(<<19>>,            dec(<<61, 125>>))].

parse_data_test() ->
    ?assertEqual({ok, [], <<"Hello World">>}, parse_data("", [[114,143,150,150,153,74,129,153,156,150,142], "=yend"])).
