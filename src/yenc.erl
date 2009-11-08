-module (yenc).
-include_lib("records.hrl").
-export ([parse/2,decode/1]).

parse(Header, Lines) ->
    {Meta, Data} = parse_data(Header, Lines),
    Bin  = decode(Data),
    {ok, Meta, Bin}.

parse_data(Header, ["=ypart "++PartInfo|Lines]) ->
    Meta = parse_header(Header),
    PartMeta = parse_header(PartInfo),
    Data = parse_body(Lines),
    {lists:append(Meta, PartMeta),Data};
parse_data(Header, Lines) ->
    Meta = parse_header(Header),
    Data = parse_body(Lines),
    {Meta,Data}.

parse_header(Header) ->
    case string:str(Header, "name=") of
        0 -> parse_header_without_name(Header);
        Index ->
            [parse_name(string:substr(Header, Index))|
             parse_header_without_name(string:substr(Header, 1, Index-1))]
    end.

parse_header_without_name(Header) ->
    [ list_to_tuple(string:tokens(X, "=")) || X<-string:tokens(Header, " ")].   
parse_name(Name) ->
    list_to_tuple(string:tokens(string:strip(Name), "=")).
    


%% collect data body up to ending =yend line
parse_body(Lines)                   -> parse_body(Lines, []).
parse_body([],Acc)                 -> lists:reverse(Acc);
parse_body(["=yend"++_|_], Acc)     -> lists:reverse(Acc);
parse_body(["=ypart"++_|Rest], Acc) -> parse_body(Rest, Acc);
parse_body([Line|Rest], Acc)        -> parse_body(Rest, [Line|Acc]).
    
decode(List) when is_list(List) -> decode(list_to_binary(List));
decode(<<$=, X, Rest/binary>>)  -> <<(X-106):8, (decode(Rest))/binary>>;
decode(<<X, Rest/binary>>)      -> <<(X-42):8, (decode(Rest))/binary>>;
decode(<<>>)                    -> <<>>.

-include_lib("eunit/include/eunit.hrl").

decode_test_() ->
    [?_assertEqual(<<"Hello World">>, decode(<<114,143,150,150,153,74,129,153,156,150,142>>)),
     ?_assertEqual(<<19>>,            decode(<<61, 125>>))].

parse_test_() ->
     ?_assertEqual({ok,
                    [{"name", "foobar with spaces.txt"}, {"part", "1"}, {"size", "123"}, {"begin", "1"}, {"end", "11250"}],
                    <<>>},
                   parse("part=1 size=123 name=foobar with spaces.txt", ["=ypart begin=1 end=11250",[],"=yend"])).
