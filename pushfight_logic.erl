-module(pushfight_logic).
-export([start/0]).

-type player() :: player1 | player2 | null.
-type token() :: square | round.
-record(piece, {player::player(),
                type::token(),
                anchored=false::boolean()}).

start() ->
    KeyValueList = [{a1, off}, {b1, empty}, {c1, empty}, {d1, off},
                    {a2, off}, {b2, #piece{player=player1,type=square}}, {c2, empty}, {d2, empty},
                    {a3, empty}, {b3, #piece{player=player2,type=round}}, {c3, empty}, {d3, empty},
                    {a4, empty}, {b4, empty}, {c4, empty}, {d4, empty}],
    StateDict = orddict:from_list(KeyValueList),
    ok.

move(State, Pos1, Pos2) ->
    ok.
