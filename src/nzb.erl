-module(nzb).
-export([parse/1]).
-include_lib("xmerl/include/xmerl.hrl").

parse(NZB) when is_binary(NZB) -> parse(binary_to_list(NZB));
parse(NZB) ->
    { Xml, _Rest } = xmerl_scan:string(NZB),
    Groups     = get_text("//file/groups/group/text()", Xml),
    MessageIds = get_text("//file/segments/segment/text()", Xml),
    { ok, Groups, MessageIds }.

get_text(XpathString, Xml) ->
    [ Text || #xmlText{value=Text} <- xmerl_xpath:string(XpathString, Xml) ].

-include_lib("eunit/include/eunit.hrl").

parse_nzb_test() ->
    {ok, Xml} = file:read_file("../test/data/nzb.xml"),
    { ok, Groups, MessageIds } = parse(Xml),
    ?assertEqual(Groups, ["alt.binaries.newzbin",
                          "alt.binaries.mojo"]),
    ?assertEqual(MessageIds, ["123456789abcdef@news.newzbin.com",
                              "987654321fedbca@news.newzbin.com"]).
