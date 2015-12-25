# Pushfight Server

A simple, scalable [Pushfight](http://pushfightgame.com/) server written in Erlang.


### Terminology & Notation

In this section, we introduce the terminology and notation that we
used to implement Pushfight.


**Squares** are the large pushing pieces. **Rounds** are the round pieces
unable to push. The **anchor** is the piece that determines the last player to
have made a move.

We label the board in a manner inspired from Chess's algebraic notation:

```
   a b c d
8   ┌─┬─┐   8
  ╓─┼─┼─┤
  ╟─┼─┼─┼─╖
  ╟─┼─┼─┼─╢
  ╟─┼─┼─┼─╢
  ╟─┼─┼─┼─╢
  ╙─┼─┼─┼─╢
    ├─┼─┼─╜
1   └─┴─┘   1
   a b c d
```

Where rows are numbered from `1` to `8` starting from the bottom, and files
are `a` to `d` left to right. For example, moving a piece from the bottom to
the top on the leftmost row is notated as `a3 a7`.


We extend this notation for void squares, where pieces may fall:

```
   a b c d
9   ┏ ┳ ┓   9
8 ┏ ╃─╀─╄ ┓ 8
  ╓─┼─┼─┤ ┫
  ╟─┼─┼─┼─╖
  ╟─┼─┼─┼─╢
  ╟─┼─┼─┼─╢
  ╟─┼─┼─┼─╢
  ╙─┼─┼─┼─╢
  ┣ ┽─┼─┼─╜
1 ┗ ╅─╁─╆ ┛ 1
0   ┗ ┻ ┛   0
   a b c d
```

For example, we can know a player loses because their piece was pushed into
position `b0`.

### Protocol Format

Because why would I use JSON? What is interoperability? What's a standard? If
you want JSON, go implement it yourself.


Game state is sent to clients with space separated values.

```
Winner Turn P1Square1 P1Square2 P1Square3 P1Round1 P1Round2 P2Square1 P2Square2 P2Square3 P2Round1 P2Round2
```

Where `Winner` is the current winner, either `null`, `player1`, or `player2`. 
`Turn` is the current turn number.
`P1Square1` is the position of player1's square number 1.   
