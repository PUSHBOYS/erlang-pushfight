# Pushfight Server

A simple, scalable Pushfight server written in Erlang.

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

Where rows are numbered from `1` to `8` starting from the bottom, and ranks
are `a` to `d` left to right.

For example, moving a piece from the bottom to the top on the leftmost
row is notated as `a3 a7`.
