-module(nzb_config).

-export([
         % Setup
         start/0, start/1, stop/0,
         
         % Accessors
         config/0,
         thread_count/0, bandwidth_limit/0,
         skip_pars/0, store_by_group/0,
         usenet/0, newzbin/0, server/0,
         groups/0, groups/1
        ]).

% Setup config ETS store
start() ->
    start("../test/data/config.sample.xml").
start(FileName) ->
    case is_started() of
        true  -> ok;
        false -> force_start(FileName)
    end.

stop() ->
    ets:delete(config).

% Full config
config() ->
    start(),
    ets:lookup_element(config, config, 2).

% Accessors
thread_count()    -> get_value('thread-count').
bandwidth_limit() -> get_value('bandwidth-limit').
skip_pars()       -> get_value('skip-pars').
store_by_group()  -> get_value('store-by-group').

usenet()  -> get_value(usenet).
newzbin() -> get_value(newzbin).
server()  -> get_value(server).

groups() -> [ {Title, Values} || {group, Title, Values} <- config() ].

groups(Title) -> get_value(Title, groups()).

%% Private

get_value(Key)       -> proplists:get_value(Key, config()).
get_value(Key, List) -> proplists:get_value(Key, List).

% Helpers for starting service

force_start(FileName) ->
    ets:new(config, [set, named_table]),
    Config = parse_xml(FileName),
    case ets:insert(config, {config, Config}) of
        true  -> ok;
        false -> {error, could_not_insert}
    end.

% Has the config store been started?
is_started() ->
    case ets:info(config) of
        undefined -> false;
        _         -> true
    end.

% http://arandomurl.com/arandomurl/_design/sofa/_show/post/Simple-XML-in-Erlang
parse_xml(FileName) ->
    {Xml,_Rest} = xmerl_scan:file(FileName, [{space, normalize}]),
    {_,_,Config} = strip_whitespace(xmerl_lib:simplify_element(Xml)),
    normalize(Config).

strip_whitespace({El,Attr,Children}) ->
    NChild = [ X || X <- Children, X =/= " " ],
    Ch = lists:map(fun strip_whitespace/1, NChild),
    {El,Attr,Ch};
strip_whitespace(String) -> String.

% Normalize converts the {Element, Attribute, Children} list parsed from the XML into final values.

% Normalize group titles
normalize({group, [{title, Title}], List}) -> {group, Title, normalize(List)};

% Normalize empty values to true
normalize({Name, [], []}) -> {Name, true};

% Normalize true/false values
normalize({Name, [], ["false"]}) -> {Name, false};
normalize({Name, [], ["true"]})  -> {Name, true};

% Recurse into values
normalize({Name, [], [Value]}) when is_tuple(Value) ->
    {Name, [normalize(Value)]};

% Integer or recurse into values
normalize({Name, [], [Value]}) -> 
    case string:to_integer(Value) of
        {error, _} -> {Name, normalize(Value)};
        {Int, []} -> {Name, Int}
    end;

% Elements without and with attributes
normalize({Name, [], List})    -> {Name, normalize(List)};
normalize({Name, Attrs, List}) -> {Name, Attrs, normalize(List)};

% Base case
normalize(Int) when is_integer(Int) -> Int;
normalize(List) -> [ normalize(Item) || Item <- List ].
    
% Tests

-include_lib("eunit/include/eunit.hrl").

thread_count_test() ->
    ?assertEqual(1, thread_count()).

bandwidth_limit_test() ->
    ?assertEqual(0, bandwidth_limit()).

skip_pars_test() ->
    ?assertEqual(true, skip_pars()).

store_by_group_test() ->
    ?assertEqual(false, store_by_group()).

usenet_test() ->
    ?assertEqual([{host,     "somehost"},
                  {port,     119},
                  {secure,   false},
                  {login,    "somelogin"},
                  {password, "somepass"}], usenet()).
                   
newzbin_test() ->
    ?assertEqual([{login,    "somelogin"},
                  {password, "somepass"}], newzbin()).

server_test() ->
    ?assertEqual([
                  {login,      "somelogin"},
                  {password,   "somepass"},
                  {port,       119},
                  {streamPort, 119},
                  {streamPath, "/path/to/stream/folder"}], server()).

groups_test() ->
    ?assertEqual([{'save-directory',"/home/ming/tmp/nzb/Music"},
                  {'par-repair',    true},
                  {unrar,           true},
                  {'mp3-rename',    [{pattern,
                                      "${artist}/${album}/${number} - ${title}"}]}],
                 groups("Music")).
