-module(pushfight).
-export([start/1, init/1]).

-type player() :: player1 | player2 | null.
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


start(Port) ->
    socket_server:start(?MODULE, Port, {?MODULE, init}).


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
    loop(Socket, {null, 0, Pieces}).


loop(Socket, State) ->
    case gen_tcp:recv(Socket, 0) of
	{ok, Input} ->
            Commands = parse_commands(Input),
            New_state = turn(Commands, State),
            State_string = state2string(New_state),
            gen_tcp:send(Socket, State_string),
            loop(Socket, New_state);
	{error, closed} ->
	    ok
    end.


state2string({Winner, Turn, Pieces}) ->
    Positions = [Piece#piece.position || Piece <- Pieces],
    io_lib:format("~w ~w ~w ~w ~w ~w ~w ~w ~w ~w ~w ~w~n",
                  [Winner|[Turn|Positions]]).


parse_commands(Input) -> % TODO: string:tokens
    case bitstring_to_list(Input) of
        [S11,S12,S21,S22,S31,S32,R11,R12,R21,R22|_Tail] ->
            [{list_to_atom([S11,S12]),
              list_to_atom([S21,S22]),
              list_to_atom([S31,S32]),
              list_to_atom([R11,R12]),
              list_to_atom([R21,R22])}]; % init command is always solo
        [R1,F1,_,R2,F2,_|Tail] ->
            Action = {list_to_atom([R1,F1]), list_to_atom([R2,F2])},
            [Action|parse_commands(Tail)];
        "end" ->
            []
    end.


turn(Commands, State={_, Turn, _}) ->
    case Commands of
        [] ->
            State; % TODO: raise error
        [Command] when Turn =:= 0 ->
            play({init, Command}, State);
        [Command] ->
            play({push, Command}, State);
        [Command|Tail] ->
            New_state = play({move, Command}, State),
            turn(Tail, New_state)
    end.


play(Command, {Winner, Turn, Pieces}) ->
    case Command of
        {init, {S1, S2, S3, R1, R2}} ->
            [P11,P12,P13,P14,P15,P21,P22,P23,P24,P25] = Pieces,
            New_pieces = case Turn of
                0 ->
                    [P11#piece{position=S1},
                     P12#piece{position=S2},
                     P13#piece{position=S3},
                     P14#piece{position=R1},
                     P15#piece{position=R2},
                     P21,P22,P23,P24,P25];
                1 ->
                    [P11,P12,P13,P14,P15,
                     P21#piece{position=S1},
                     P22#piece{position=S2},
                     P23#piece{position=S3},
                     P24#piece{position=R1},
                     P25#piece{position=R2}]
            end,
            {Winner, Turn + 1, New_pieces};
        {push, {_Src, _Dst}} ->
            New_pieces = Pieces, % TODO: implement push
            New_winner = check_winner(New_pieces),
            {New_winner, Turn + 1, New_pieces};
        {move, {Src, Dst}} ->
            % TODO: validation
            New_pieces = [
            case Piece#piece.position of
                Src -> Piece#piece{position=Dst};
                _ -> Piece
            end || Piece <- Pieces],
            {Winner, Turn, New_pieces}
    end.


check_winner(_Pieces) ->
    Positions = [Piece#piece.position || Piece <- Pieces],
    Off_pieces = [Piece || Piece <- Positions, Positions == a1 orelse
                                                Positions == a2 orelse
                                                Positions == b0 orelse
                                                Positions == c0 orelse
                                                Positions == d1 orelse
                                                Positions == d2 orelse
                                                Positions == a8 orelse
                                                Positions == a9 orelse
                                                Positions == b9 orelse
                                                Positions == c9 orelse
                                                Positions == d7 orelse
                                                Positions == d8],
    case Off_pieces of
        [] ->
            null;
        [Off_piece] ->
            Off_piece#piece.player
    end.
