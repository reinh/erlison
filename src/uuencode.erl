-module(uuencode).

-export([dec/1]).

dec(Text) -> << <<(X-32):6>> || <<X:8>> <= Text >>.

-include_lib("eunit/include/eunit.hrl").

dec_test() ->
    ?assertEqual(<<"Cat">>, dec(<<"0V%T">>)).
