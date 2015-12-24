-module(pushfight).
-export([start/0, init/1]).

-type piece() :: empty | square | round | anchor.
-record(position, {valid::boolean(), status::piece()}).

% echo_server specific code
start() ->
    socket_server:start(?MODULE, 7000, {?MODULE, init}).


init(Socket) ->
    Gamestate = [],
    loop(Socket).


loop(Socket) ->
    case gen_tcp:recv(Socket, 0) of
	{ok, Data} ->
	    gen_tcp:send(Socket, Data), % Currently just an echo server.
	    loop(Socket);
	{error, closed} ->
	    ok
    end.
