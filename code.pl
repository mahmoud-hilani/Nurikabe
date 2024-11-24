:- dynamic fxd_cell/3.
:- dynamic solve_cell/3.


% Defining the grid size
grid_size(7, 7) .

% Facts Representing the Initial Grid
fxd_cell(1, 2, 3).
fxd_cell(1, 6, 1).
fxd_cell(3, 1, 2).
fxd_cell(3, 4, 1).
fxd_cell(5, 2, 1).
fxd_cell(5, 5, 2).
fxd_cell(6, 3, 2).
fxd_cell(7, 1, 1).
fxd_cell(7, 5, 1).
fxd_cell(7, 7, 6).


solve_cell(1, 4,green).
solve_cell(1, 3,green).
solve_cell(3, 2,green).
solve_cell(3, 6,green).
solve_cell(3, 7,green).
solve_cell(4, 7,green).
solve_cell(5, 4,green).
solve_cell(5, 7,green).
solve_cell(6, 7,green).
solve_cell(7, 3,green).

add_land(Row,Col):-
    grid_size(Rows, Cols),
    between(1,Rows,Row),
    between(1,Cols,Col),
    \+(fxd_cell(Row,Col,_);
      solve_cell(Row,Col,green)),
    assert(solve_cell(Row,Col,green)),
    print_grid,
  (    solve_cell(Row,Col,blue)-> retract(solve_cell(Row,Col,blue)));true.


remove_land(Row,Col):-
    retract(solve_cell(Row,Col,green)),
    assert(solve_cell(Row,Col,blue)),
        print_grid.

is_empty_cell(Row, Col) :-
    \+ solve_cell(Row, Col, _),                 % Not solved (land or sea)
    \+ fxd_cell(Row, Col, _).

restart:-
    retractall(solve_cell(_,_,_)),
        print_grid.

print_grid :-
    grid_size(Rows, Cols),
    print_border(Cols), nl,
    between(1, Rows, Row),
    print_row(Row, Cols), nl,
    fail.
print_grid :-
        grid_size(_, Cols),
    print_border(Cols), nl, nl.


print_border(Cols) :-
    write('+'),
    line(Cols ).

print_row(Row, Cols) :-
    write('|'),
    between(1, Cols, Col),
    (   fxd_cell(Row, Col, Num) -> writef(' %d |', [Num]);
        solve_cell(Row, Col, green) -> write(' # |');
        solve_cell(Row, Col, blue) -> write(' ~ |');
        write('    |')
    ),

    fail.
print_row(_, _).


line( 0).
line( N) :-
    N > 0,
    write('--+'),
    N1 is N - 1,
    line( N1).


    %get the neighbors

adjacent(Row, Col, RowN, ColN) :-
    grid_size(MaxRow, MaxCol),
    (
        (RowN is Row - 1, ColN is Col, RowN > 0);
        (RowN is Row + 1, ColN is Col, RowN =< MaxRow);
        (RowN is Row, ColN is Col - 1, ColN > 0);
        (RowN is Row, ColN is Col + 1, ColN =< MaxCol)
    ).

add_sea_cells:-
       grid_size(Rows, Cols),
     forall(  ( between(1,Rows,Row),
    between(1,Cols,Col),
   is_empty_cell(Row, Col)),
           assert(solve_cell(Row,Col,blue))),
     print_grid.

%  Finding all cells using findall
all_sea_cells(SeaCells) :-
    findall((Row, Col),
           solve_cell(Row, Col,blue),
            SeaCells).


 all_land_cells(LandCells):-
       findall((Row, Col),
           solve_cell(Row, Col,green),
            LandCells).

all_fixed_cells(FixedCells):-
       findall((Row, Col,Num),
           fxd_cell(Row, Col,Num),
            FixedCells).

% 1. check if the all sea cells are connected
one_sea :-
    solve_cell(StartRow, StartCol,blue),!,
    bfs_sea(StartRow, StartCol, ConnectedSeaCells),
    sort(ConnectedSeaCells,SConnectedSeaCells),
    all_sea_cells(SeaCells),
    sort(SeaCells,SSeaCells),
SConnectedSeaCells = SSeaCells.

%list of connected sea cells for a cell

bfs_sea(StartRow, StartCol, ConnectedSeaCells) :-
    bfs_sea_helper([(StartRow, StartCol)], [],  ConnectedSeaCells).

bfs_sea_helper([], Visited, Visited).
bfs_sea_helper([(Row, Col) | Queue], Visited,  ConnectedSeaCells) :-
    (
                (
            solve_cell(Row, Col,blue),
            findall((RowN, ColN),
              (adjacent(Row, Col, RowN, ColN),
              solve_cell(RowN, ColN,blue),
           \+ member((RowN, ColN), Visited)),
                    Neighbors),
            append(Queue, Neighbors, NewQueue),
            bfs_sea_helper(NewQueue, [(Row, Col) | Visited],  ConnectedSeaCells)
        )
    ).


% 2. NO 2x2 blocks of sea cells in the entire grid
no_2_by_2_sea_blocks :-
   \+ (
        grid_size(Rows,Cols),
                Rows1 is Rows -1,
                Cols1 is Cols -1,


        between(1, Rows1 , Row),
        between(1, Cols1, Col),
        is_2x2_sea_block(Row, Col)
    ).



%  NO 2x2 blocks of sea cells in one seacell

is_2x2_sea_block(Row, Col) :-
    Row1 is Row +1 ,
    Col1 is Col +1 ,
    solve_cell(Row,Col,blue),
    solve_cell(Row1,Col,blue),
    solve_cell(Row,Col1,blue),
    solve_cell(Row1,Col1,blue).


%list of connected land cells for a cell

bfs_island(StartRow, StartCol, ConnectedSeaCells) :-
    bfs_island_helper([(StartRow, StartCol)], [],  ConnectedSeaCells).

bfs_island_helper([], Visited, Visited).
bfs_island_helper([(Row, Col) | Queue], Visited,  ConnectedSeaCells) :-
    (

        (
          (    solve_cell(Row, Col, green);
      fxd_cell(Row, Col,_)),
            findall((RowN, ColN),
                    (adjacent_land(Row, Col, RowN, ColN),
                     \+ member((RowN, ColN), Visited)),
                    Neighbors),
            append(Queue, Neighbors, NewQueue),
            bfs_island_helper(NewQueue, [(Row, Col) | Visited],  ConnectedSeaCells)
        )
    ).

% 3-4 check island has one fxd cell and the land cells number is equal
% to the fxd Num
island_equals_one_fixed :-
   forall( fxd_cell(Row,Col,Num),
   (    bfs_island(Row,Col, Island),
           count_fixed_cells(Island, 1),
   length(Island, Num))).


% 5 make sure there is no island without fixed cell

lands_equals_fixeds:-
all_land_cells(SolveCells),
    all_fixed_cells(FixedCells),
    length(SolveCells,Slength),
        length(FixedCells,Flength),
        Length is Slength+Flength,
        sum_fixed_cells(FixedCells,Length).

%fixed cell helper method
%
%
%how many fixed cell in a list

count_fixed_cells([], 0).
count_fixed_cells([(Row, Col) | Tail], Count) :-
    count_fixed_cells(Tail, Count1),
    (fxd_cell(Row, Col, _) ->
        Count is Count1 + 1
    ;   Count is Count1).


%the sum of fixed cell Num in a list


sum_fixed_cells([], 0).
sum_fixed_cells([(_, _,Num) | Tail], Count) :-
    sum_fixed_cells(Tail, Count1),
    (          Count is Count1 + Num
    ;   Count is Count1).


solved :- one_sea,
    no_2_by_2_sea_blocks,
    island_equals_one_fixed,
    lands_equals_fixeds.


print_list([]).

% Recursive case: A non-empty list has the head to print and then print the rest of the list
print_list([Head|Tail]) :-
    write(Head), nl,  % Print the head of the list
    print_list(Tail).

print_land:- all_land_cells(T),
    print_list(T).
%forall(Condition, Action) :-
%    \+ (Condition, \+ Action).




%AI
%
%1. surround 1 with ~

fixed_one:- forall( fxd_cell(Row,Col,1),
           (    findall((RowN,ColN),adjacent(Row,Col,RowN,ColN),Neighbors),
            add_sea(Neighbors))).

% asserting list to a seacell
%
add_sea([]).
add_sea([(Row,Col)|T]):-
  (  \+ is_empty_cell(Row, Col)) -> add_sea(T) ;
    assert(solve_cell(Row,Col,blue)),
    print_grid,
          add_sea(T).
% seperate fxd cell with 1 space
fxd_separation :-
    findall((Row1, Col1, Num1), fxd_cell(Row1, Col1, Num1), Clues),
    forall(
        (
            member((Row1, Col1, _), Clues),
            member((Row2, Col2, _), Clues),
            (
                (Row1 =:= Row2, abs(Col1 - Col2) =:= 2) ;
                (Col1 =:= Col2, abs(Row1 - Row2) =:= 2)               )
        ),
        (
            BetweenRow is (Row1 + Row2) // 2,
            BetweenCol is (Col1 + Col2) // 2,
           add_sea([(BetweenRow,BetweenCol)])
        )
    ).

diagonally_adjacent_constraint :-
    findall((Row1, Col1, Num1), fxd_cell(Row1, Col1, Num1), Clues),
    forall(
        (
            member((Row1, Col1, _), Clues),
            member((Row2, Col2, _), Clues),
            (abs(Row1 - Row2) =:= 1, abs(Col1 - Col2) =:= 1)
        ),
        (
            findall((AdjRow, AdjCol), (
                adjacent(Row1, Col1, AdjRow, AdjCol),
                adjacent(Row2, Col2, AdjRow, AdjCol),
                \+ solve_cell(AdjRow, AdjCol, _)
            ), AdjCells),
            add_sea(AdjCells)
        )
    ).
%empty cell surronded by sea
empty_surrounded_by_sea:-
    grid_size(Rows, Cols),
    forall(
        (
            between(1, Rows, Row),
            between(1, Cols, Col),
           is_empty_cell(Row, Col),
        findall((RowN, ColN), adjacent(Row, Col, RowN, ColN), Neighbors),
            all_sea(Neighbors)
        ),
         add_sea([(Row,Col)])
    ).

%check if all list is sea
all_sea([]).
all_sea([(Row, Col) | Rest]) :-
    solve_cell(Row, Col, blue),
    all_sea(Rest).


adjacent_empty_islands([], []).                  % Base case: empty input list, empty output

adjacent_empty_islands([(Row, Col) | Rest], Result) :-
    findall((RowN, ColN), (
       adjacent_empty_land(Row, Col, RowN, ColN)
    ), AdjacentEmpty),
    adjacent_empty_islands(Rest, RestResult),   % Recurse on the rest of the list
    append(AdjacentEmpty, RestResult, Result).  % Combine results


adjacent_empty_land(Row, Col, RowN, ColN) :-
    adjacent(Row, Col, RowN, ColN),
    is_empty_cell(RowN, ColN).

adjacent_sea(Row, Col,RowN, ColN) :-
   (    adjacent(Row, Col, RowN, ColN),
    solve_cell(RowN, ColN, blue)).

adjacent_land(Row, Col,RowN, ColN) :-
    adjacent(Row, Col, RowN, ColN),
   (    solve_cell(RowN, ColN, green);
      fxd_cell(RowN, ColN,_)).

solving:-
        fixed_one,
    fxd_separation,
    diagonally_adjacent_constraint,
   empty_surrounded_by_sea,
   corner_sea,
   corner_land,
  wrap_complete_island,
   fail.
solving:-true.

corner_sea:-

                solve_cell(Row, Col, blue),
                   findall((RowN, ColN), adjacent_sea(Row, Col,RowN, ColN), SeaNeighbors),
                length(SeaNeighbors, 0),
           findall((RowN, ColN), adjacent_empty_land(Row, Col,RowN, ColN), EmptyNeighbors),
            length(EmptyNeighbors, 1),
                        EmptyNeighbors=[(NRow,NCol)|_],
                    add_sea(EmptyNeighbors),
                        expand_corner_sea(NRow,NCol).


corner_sea:- true.

%expand_corner(_,_).
expand_corner_sea(Row,Col):-
     findall((RowN, ColN), adjacent_sea(Row, Col,RowN, ColN), SeaNeighbors),
                length(SeaNeighbors, 1),
           findall((RowN, ColN), adjacent_empty_land(Row, Col,RowN, ColN), EmptyNeighbors),
            length(EmptyNeighbors, 1),
            EmptyNeighbors=[(NRow,NCol)|_],
                    add_sea([(NRow,NCol)]),
                        expand_corner_sea(NRow,NCol).


corner_land:-  fxd_cell(Row, Col, Num),
                   findall((RowN, ColN), adjacent_land(Row, Col,RowN, ColN), LandNeighbors),
                length(LandNeighbors, 0),
           findall((RowN, ColN), adjacent_empty_land(Row, Col,RowN, ColN), EmptyNeighbors),
            length(EmptyNeighbors, 1),
                        EmptyNeighbors=[(NRow,NCol)|_],
                    add_land(NRow,NCol),
                    expand_corner_land(NRow,NCol,Num,2).
corner_land:-  true.

expand_corner_land(Row,Col,Num,I) :-
               findall((RowN, ColN), adjacent_empty_land(Row, Col,RowN, ColN), EmptyNeighbors),
          (     Num>I ->( length(EmptyNeighbors, 1),
                        EmptyNeighbors=[(NRow,NCol)|_],
                    add_land(NRow,NCol),
                      I1 is I +1,
                      expand_corner_land(NRow,NCol,Num,I1)

                    );
          EmptyNeighbors=[(NRow,NCol)|_],
                    add_land(NRow,NCol)
).


wrap_complete_island :-
  fxd_cell(Row,Col,Num),
       bfs_island(Row,Col, Island),
           count_fixed_cells(Island, 1),
   length(Island, Num),
                 adjacent_empty_islands( Island,IslandEmptyNeighbors),

        add_sea(IslandEmptyNeighbors),fail.

wrap_complete_island :- true.


prevent_2x2_sea_by_filling :-
    grid_size(Rows, Cols),


            between(1, Rows, Row),
            between(1, Cols, Col),
        \+( solve_cell(Row, Col, _);fxd_cell(Row,Col,_)),
        add_sea([(Row,Col)])

        ,
           (    creates_2x2_sea_if_filled(Row, Col) ->
       (    add_land(Row, Col)) ; retract(solve_cell(Row,Col,blue))),
        fail.

prevent_2x2_sea_by_filling:- true.
creates_2x2_sea_if_filled(Row, Col) :-
    grid_size(Rows, Cols),
    (
        (Row =< Rows, Col =< Cols, is_2x2_sea_block(Row, Col)) ;
        (Row > 1, Col =< Cols, is_2x2_sea_block(Row - 1, Col)) ;
        (Row =< Rows, Col > 1, is_2x2_sea_block(Row, Col - 1)) ;
        (Row > 1, Col > 1, is_2x2_sea_block(Row - 1, Col - 1))
    ).

t(Row,Col,Neighbors) :-  findall((RowN, ColN),
                    adjacent_land(Row, Col, RowN, ColN),

                    Neighbors).
