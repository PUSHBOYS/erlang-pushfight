-module(player_logic).
-export([listen/1]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

listen(Port) ->
    {ok, Socket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    accept(Socket).

accept(Socket) ->
    {ok, Client_Socket} = gen_tcp:accept(Socket),
    Pid = spawn(fun() -> loop(Client_Socket) end),
    io:format("~w~n", [Pid]),
    accept(Socket).

communicate(Socket, New_state) ->
    gen_tcp:send(Socket, New_state),
    case gen_tcp:recv(Socket, 0) of
        {ok, _Response} ->
            ok;
        {quit, _Reason} ->
            ok
    end.

loop(Socket) ->
    receive
        {From, {notify, New_state}} ->
            From ! communicate(Socket, New_state),
            loop(Socket);
        {_From, {quit, _Status}} ->
            ok
    end.
