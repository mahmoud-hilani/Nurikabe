:- dynamic fxd_cell/3.
:- dynamic solve_cell/2.

% Defining the grid size
grid_size(Rows, Cols) :-
    Rows = 7,
    Cols = 7.

% Assigning fixed cells (islands)
assign_fixed_cells :-
    fxd_cell(Row, Col),
    assertz(solve_cell(Row, Col)),
    fail.
assign_fixed_cells.

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


solve_cell(1, 4).
solve_cell(1, 3).
solve_cell(3, 2).
solve_cell(3, 6).
solve_cell(3, 7).
solve_cell(4, 7).
solve_cell(5, 4).

solve_cell(5, 7).
solve_cell(6, 7).

solve_cell(7, 3).


add_land(Row,Col):-
    grid_size(Rows, Cols),
    between(1,Rows,Row),
    between(1,Cols,Col),
    sea_cell(Row,Col),
    assert(solve_cell(Row,Col)),
    print_grid.

remove_land(Row,Col):-
    retract(solve_cell(Row,Col)),
        print_grid.



% 1. Finding neighbors that are land

% Printing the grid
print_grid :-
    grid_size(Rows, Cols),
    between(1, Rows, Row),
    print_row(Row, Cols),
    nl,
    fail.
print_grid.

print_row(Row, Cols) :-
    between(1, Cols, Col),
    (fxd_cell(Row, Col, Num) -> write(Num);solve_cell(Row, Col)  -> write('#'); write('~')),
    write(' '),
    fail.
print_row(_, _).

adjacent(Row, Col, RowN, ColN) :-
    grid_size(MaxRow, MaxCol),
    (
        (RowN is Row - 1, ColN is Col, RowN > 0);
        (RowN is Row + 1, ColN is Col, RowN =< MaxRow);
        (RowN is Row, ColN is Col - 1, ColN > 0);
        (RowN is Row, ColN is Col + 1, ColN =< MaxCol)
    ).
    % Check if a single sea cell has a sea neighbor

sea_cell(Row,Col):-
     grid_size(Rows, Cols),
    between(1,Rows,Row),
    between(1,Cols,Col),
    \+ (solve_cell(Row, Col); fxd_cell(Row, Col, _)).
% Check if all sea cells have sea neighbors
%

% Check if there's a 2x2 block of sea cells at a given position (within grid boundaries)
is_2x2_sea_block(Row, Col) :-
    grid_size(MaxRow, MaxCol),  % Get grid dimensions
    Row + 1 =< MaxRow,          % Check if the block stays within the rows
    Col + 1 =< MaxCol,          % Check if the block stays within the columns
    Row1 is Row +1 ,
    Col1 is Col +1 ,
    sea_cell(Row, Col),
    sea_cell(Row1, Col),
    sea_cell(Row, Col1),
    sea_cell(Row1, Col1).

% Check if there are NO 2x2 blocks of sea cells in the entire grid
no_2_by_2_sea_blocks :-
   \+ (
        grid_size(Rows,Cols),
                Rows1 is Rows -1,
                Cols1 is Cols -1,


        between(1, Rows1 , Row),
        between(1, Cols1, Col),   % Use MaxCol1 instead of MaxCol - 1
        is_2x2_sea_block(Row, Col)
    ).

% Find connected sea cells starting from a given cell
% 1. Finding all sea cells
all_sea_cells(SeaCells) :-
    findall((Row, Col),
           sea_cell(Row, Col), % Not land
            SeaCells).



one_sea :-
    sea_cell(StartRow, StartCol),!,
    bfs_sea(StartRow, StartCol, ConnectedSeaCells),
    sort(ConnectedSeaCells,SConnectedSeaCells),
    all_sea_cells(SeaCells),
SConnectedSeaCells = SeaCells.



bfs_sea(StartRow, StartCol, ConnectedSeaCells) :-
    bfs_sea_helper([(StartRow, StartCol)], [],  ConnectedSeaCells).

bfs_sea_helper([], Visited, Visited).  % Base case: no more cells to visit
bfs_sea_helper([(Row, Col) | Queue], Visited,  ConnectedSeaCells) :-
    (
        member((Row, Col), Visited) ->
  bfs_sea_helper(Queue, Visited,  ConnectedSeaCells) % Already visited, skip
        ;
        (
            sea_cell(Row, Col),
            findall((RowN, ColN),
                    (adjacent(Row, Col, RowN, ColN),
                     sea_cell(RowN, ColN),
                     \+ member((RowN, ColN), Visited)),
                    Neighbors),
            append(Queue, Neighbors, NewQueue),
            bfs_sea_helper(NewQueue, [(Row, Col) | Visited],  ConnectedSeaCells)
        )
    ).


% 2. Finding a group (island) starting from a cell
find_island(Row, Col, Island) :-
    find_island_helper(Row, Col, [], Island). % Initialize Visited as []

find_island_helper(Row, Col, Visited, Island) :-
    setof((RowN, ColN), find_island_helper2(Row, Col, Visited, RowN, ColN), Island).

find_island_helper2(Row, Col, _, Row, Col) :-
    (solve_cell(Row, Col); fxd_cell(Row, Col, _)).

find_island_helper2(Row, Col, Visited, RowN, ColN) :-
    (solve_cell(Row, Col); fxd_cell(Row, Col, _)),
    adjacent(Row, Col, RowX, ColX),
    (solve_cell(RowX, ColX); fxd_cell(RowX, ColX, _)),
    \+ member((RowX, ColX), Visited),
    find_island_helper2(RowX, ColX, [(RowX, ColX) | Visited], RowN, ColN).

% 5. Checking if each island has exactly one fixed cell
%


island_number_equals_one_fixed :-
   forall( fxd_cell(Row,Col,Num),
   (    find_island(Row,Col, Island),
           count_fixed_cells(Island, 1),
   length(Island, Num))).

% 6. Checking if the size of each island equals the number in the fixed cell
island_number_equals_size :-
    forall(fxd_cell(Row, Col, Num),
           (find_island(Row, Col, Island),
            length(Island, Num))).


count_fixed_cells([], 0).         % Base case: empty list has 0 fixed cells
count_fixed_cells([(Row, Col) | Tail], Count) :-
    count_fixed_cells(Tail, Count1),  % Recurse first to get count from the tail
    (fxd_cell(Row, Col, _) ->  % Check if the cell is a fixed cell
        Count is Count1 + 1    % If fixed, increment the count
    ;   Count is Count1).       % If not fixed, keep the count the same


solved :- one_sea,
    no_2_by_2_sea_blocks.













no_2_by_2_sea_blocks2 :-
    forall(
        (
            grid_size(Rows, Cols),
        Rows is Rows -1,
                Cols is Rows -1,

            between(1, Rows , Row),
            between(1, Cols , Col)
        ),
        \+ is_2x2_sea_block(Row, Col)  % Succeed if NOT a 2x2 sea block
    ).


has_sea_neighbor(Row, Col) :-
    adjacent(Row, Col, RowN, ColN),
    sea_cell(RowN, ColN).  % Neighbor is not land

all_sea_cells_have_neighbors :-
    \+ (
        grid_size(Rows, Cols),
        between(1, Rows, Row),
        between(1, Cols, Col),
         sea_cell(Row,Col), % Is sea cell
        \+ has_sea_neighbor(Row, Col)                     % Doesn't have sea neighbor
    ).
