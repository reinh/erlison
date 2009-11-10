-module(nntp).

-export([get_article/1]).

-define(Host, "news.giganews.com").
-define(Port, 119).

get_article(ArticleID) ->
    {ok, Socket} = connect(),
    {ok, _} = auth(Socket),
    article(Socket, ArticleID).

connect() ->
    {ok,Socket} = gen_tcp:connect(?Host,?Port,[binary, {packet, 0}, {active, false}]),
    {ok, <<"200", _Rest/binary>>} = recv(Socket),
    {ok, Socket}.

% TODO: read auth info from config
auth(Socket) ->
    send(Socket, "AUTHINFO USER ..."),
    recv(Socket),
    send(Socket, "AUTHINFO PASS ..."),
    recv(Socket).

article(Socket, ArticleID) ->
    send(Socket, "ARTICLE <"++ArticleID++">"),
    recv(Socket).

send(Socket,Msg) ->
    gen_tcp:send(Socket, Msg++"\r\n").

recv(Socket) ->
    gen_tcp:recv(Socket,0).
