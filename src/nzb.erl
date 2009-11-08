-module(nzb).
-export([parse/1]).
-include_lib("xmerl/include/xmerl.hrl").

parse(NZB) when is_binary(NZB) -> parse(binary_to_list(NZB));
parse(NZB) ->
    {Xml, _Rest} = xmerl_scan:string(NZB, [{space, normalize}]),
    FileElement = lists:last(xmerl_xpath:string("//file", Xml)),
    {file, Attributes, _} = xmerl_lib:simplify_element(FileElement),
    Subject    = proplists:get_value(subject, Attributes),
    Groups     = get_text("//file/groups/group/text()", Xml),
    MessageIds = get_text("//file/segments/segment/text()", Xml),
    {ok, Subject, Groups, MessageIds}.

get_text(XpathString, Xml) ->
    [ Text || #xmlText{value=Text} <- xmerl_xpath:string(XpathString, Xml) ].

-include_lib("eunit/include/eunit.hrl").

parse_nzb_test() ->
    {ok, Xml} = file:read_file("../test/data/nzb.xml"),
    {ok, Subject, Groups, MessageIds} = parse(Xml),
    ?assertEqual(Subject, "Here's your file!  abc-mr2a.r01 (1/2)"),
    ?assertEqual(Groups, ["alt.binaries.newzbin",
                          "alt.binaries.mojo"]),
    ?assertEqual(MessageIds, ["123456789abcdef@news.newzbin.com",
                              "987654321fedbca@news.newzbin.com"]).
