-module(pushfight_logic).
-export([init/2]).

-type player() :: player1 | player2 | null.
-type token() :: square | round.
-record(piece, {player::player(),
                type::token(),
                anchored=false::boolean()}).

init(Player1, Player2) ->
    Pid1 = spawn_link(fun() -> pre_game(Player1, self()) end),
    Pid2 = spawn_link(fun() -> pre_game(Player2, self()) end),
    receive
        {Pid1, State1} ->
            receive
                {Pid2, State2} ->
                    ok
            end;
        {Pid2, State2} ->
            receive
                {Pid1, State1} ->
                    ok
            end
    end,
    loop(State1++State2, 0, Player1, Player2).

pre_game(Player, Parent) ->
    Player ! {self(), notify},
    receive
        {Player, State} ->
            Parent ! State
    end,
    exit(normal).

loop(State, Turn, Player1, Player2) ->
    if
        Turn rem 2 == 1 ->
            Player1 ! {self(), notify};
        Turn rem 2 == 0 ->
            Player2 ! {self(), notify}
    end,
    receive
        {Player1, New_state} ->
            ok;
        {Player2, New_state} ->
            ok
    end,
    loop(New_state, Turn+1, Player1, Player2).
