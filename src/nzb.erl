-module(nzb).
-export([parse/1]).
-include_lib("xmerl/include/xmerl.hrl").

parse(NZB) when is_binary(NZB) -> parse(binary_to_list(NZB));
parse(NZB) ->
    { Xml, _Rest } = xmerl_scan:string(NZB),
    [ #xmlText{value=Group}|_T ] =  xmerl_xpath:string("//file/groups/group/text()", Xml),
    MessageIds = [ MessageId ||#xmlText{value=MessageId} <- xmerl_xpath:string("//file/segments/segment/text()", Xml) ],
    { ok, Group, MessageIds }.

-include_lib("eunit/include/eunit.hrl").

parse_nzb_test() ->
    {ok, Xml} = file:read_file("../test/data/nzb.xml"),
    { ok, Group, MessageIds } = parse(Xml),
    ?assertEqual(Group, "alt.binaries.newzbin"),
    ?assertEqual(MessageIds, ["123456789abcdef@news.newzbin.com",
                              "987654321fedbca@news.newzbin.com"]).
