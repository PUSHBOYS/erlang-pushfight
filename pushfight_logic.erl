-module(pushfight_logic).
-export([init/2]).

-type player() :: player1 | player2 | null.
-type token() :: square | round.
-record(piece, {player::player(),
                type::token(),
                anchored=false::boolean()}).

init(Player1, Player2) ->
    spawn(fun() -> pre_game_setup(Player1, Player2) end).

pre_game_setup(Player1, Player2) ->
    Player1 ! {self(), pre_game},
    Player2 ! {self(), pre_game},
    receive
        {Player1, State1} ->
            ok
        {Player2, State2} ->
            ok;
    end,
    New_state = State1 ++ State2,
    loop(New_state, 0, Player1, Player2).

loop(State, Turn, Player1, Player2) ->
    if
        Turn mod 2 == 1 ->
            Player1 ! {self(), notify}
        _Else ->
            Player2 ! {self(), notify};
    end,
    receive
        {Player1, New_state} ->
            ok
        {Player2, New_state} ->
            ok;
    end,
    loop(New_state, Turn+1, Player1, Player2).
