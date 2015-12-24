-module(pushfight).
-export([start/0, init/1]).

-type player() :: player1 | player2.
-type token() :: square | round.
-type position() :: null | b0 | c0 |
                      a1 | b1 | c1 | d1 |
                      a2 | b2 | c2 | d2 |
                      a3 | b3 | c3 | d3 |
                      a4 | b4 | c4 | d4 |
                      a5 | b5 | c5 | d5 |
                      a6 | b6 | c6 | d6 |
                      a7 | b7 | c7 | d7 |
                      a8 | b8 | c8 | d8 |
                           b9 | c9.

-record(piece, {player::player(),
                type::token(),
                position=null::position(),
                anchored=false::boolean()}).

% reference board
%    a b c d 
% 9   ┏ ┳ ┓
% 8 ┏ ╃─╀─╄ ┓ 9 
% 7 ╓─┼─┼─┤ ┫ 8
% 6 ╟─┼─┼─┼─╖ 7
% 5 ╟─┼─┼─┼─╢ 6
% 4 ╟─┼─┼─┼─╢ 5
% 3 ╟─┼─┼─┼─╢ 4
% 2 ╙─┼─┼─┼─╢ 3
% 1 ┣ ┽─┼─┼─╜ 2
% 0 ┗ ╅─╁─╆ ┛ 1 
%     ┗ ┻ ┛   0   
%    a b c d


start() ->
    socket_server:start(?MODULE, 7000, {?MODULE, init}).


init(Socket) ->
    Pieces = [#piece{player=player1, type=square},
              #piece{player=player1, type=square},
              #piece{player=player1, type=square},
              #piece{player=player1, type=round},
              #piece{player=player1, type=round},
              #piece{player=player2, type=square},
              #piece{player=player2, type=square},
              #piece{player=player2, type=square},
              #piece{player=player2, type=round},
              #piece{player=player2, type=round}],
    loop(Socket, Pieces, 0).


loop(Socket, Pieces, Turn) ->
    case gen_tcp:recv(Socket, 0) of
	{ok, _Data} ->
            Positions = [Piece#piece.position || Piece <- Pieces],
            State_string = io_lib:format("~d ~w ~w ~w ~w ~w ~w ~w ~w ~w ~w~n",
                                         [Turn|Positions]),
	    gen_tcp:send(Socket, State_string), % Currently just sends the gamestate.
	    loop(Socket, Pieces, Turn+1);
	{error, closed} ->
	    ok
    end.
