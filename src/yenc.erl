-module (yenc).

-export ([dec/1]).

dec (<<$=, X, Rest/binary>>) -> <<(X-106 rem 256):8, (dec(Rest))/binary>>;
dec (<<X, Rest/binary>>)     -> <<(X-42 rem 256):8, (dec(Rest))/binary>>;
dec (<<>>)                   -> <<>>.

-include_lib("eunit/include/eunit.hrl").

dec_test_() ->
    [?_assertEqual(<<"Hello World">>, dec(<<114,143,150,150,153,74,129,153,156,150,142>>)),
     ?_assertEqual(<<19>>,            dec(<<61, 125>>))].
     
