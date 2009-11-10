-module(nzb_config).

-compile(export_all).

parse(FileName) ->
    {Xml,_Rest} = xmerl_scan:file(FileName),
    {_,_,Config} = xmerl_lib:simplify_element(Xml),
    Config.

thread_count(Config) ->
    ConfigValues = [ {Name, Value} || {Name, _, Value} <- Config ],
    [S] = proplists:get_value('thread-count', ConfigValues),
    {I,_} = string:to_integer(S),
    I.

groups(Config) ->
    [{group, Atts, Value} || {group, Atts, Value} <- Config ].
