-module(gol).
-compile(export_all).

-import(gol_cell).

%% initial state is represented by an array of strings where 0 = empty cell
%% and 1 = occupied cell

start_blinker() ->
    start(["00000", "00000", "01110", "00000", "00000"]).

start_test() ->
    start(["123", "456", "789"]).

start([IS | InitialStates]) ->
    io:format("Columns = ~p, Rows = ~p~n", 
              [length(IS), 
               length([IS|InitialStates])]),

    AllCells = generate_cells(length(IS),
                              length([IS|InitialStates]),
                              lists:reverse([IS|InitialStates])),
    
    gol:assign_neighbours(AllCells, AllCells),

    spawn(fun() -> gol:loop(AllCells) end).

generate_cells(_, _, []) ->
     [];

generate_cells(TotalRows, TotalCols, [IS | InitialStates]) ->
    Cells = gol:generate_column_cells(length([IS|InitialStates]), 
                                      TotalRows, 
                                      TotalCols, 
                                      lists:reverse(IS)),
    lists:append(Cells, gol:generate_cells(TotalRows, TotalCols, InitialStates)).


state(48) ->
    dead;
state(_) ->
    living.

generate_column_cells(_, _, _, []) ->
    [];
generate_column_cells(CurrentRow, TotalRows, TotalCols, [H|T]) ->
    CurrentCol = length([H|T]),
    CellPid = spawn(fun() -> gol_cell:init_loop(CurrentRow, CurrentCol, state(H)) end), 
    [{ pid, CellPid, row, CurrentRow, col, CurrentCol } |
     generate_column_cells(CurrentRow, TotalRows, TotalCols, T)].


assign_neighbours([], _) ->
    ok;

assign_neighbours([{pid, Pid, row, Row, col, Col}|Cells], AllCells) ->
    Neighbours = neighbours(Row, Col, AllCells),
    Pid ! {set_neighbours, Neighbours},
    assign_neighbours(Cells, AllCells).

is_neighbour(Row, Col, Row, Col) -> 
    false;

is_neighbour(Row, Col, NeighbourRow, NeighbourCol) ->
    (abs(Row - NeighbourRow) =< 1) and (abs(Col - NeighbourCol) =< 1).


neighbours(Row, Col, AllCells) ->
    [Pid || {pid, Pid, _, _, _, _ } 
                <- lists:filter(
                     fun({_, _, row, R, col, C}) -> 
                             is_neighbour(Row, Col, R, C) 
                     end, 
                     AllCells)].

%% interface

tick(Pid) ->
    rpc(Pid, tick).

exit(Pid) ->
    rpc(Pid, exit).

rpc(Pid, Request) ->
    Pid ! {self(), Request},
    receive
        {Pid, Response} ->
            Response
    end.


%% main stuff

loop(AllCells) ->
    receive
        {From, tick} ->
            io:format("main loop got tick~n"),
            [Cell ! {self(), tick} || {pid, Cell, _, _, _, _} <- AllCells ],
            gol:wait_cell_update(From, AllCells, length(AllCells));
        {From, exit} ->
            [Cell ! {self(), exit} || {pid, Cell, _, _, _, _} <- AllCells ],
            From ! {self(), "exiting"};
        Any ->
            io:format("main loop got unknown message: ~p~n", [Any]),
            gol:loop(AllCells)
    end.

wait_cell_update(From, AllCells, 0) ->
    io:format("all cells have updated~n"),
    From ! {self(), "all cells have been updated"},
    gol:loop(AllCells);

wait_cell_update(From, AllCells, UpdateCounter) ->
    io:format("waiting for ~p more updates~n", [UpdateCounter]),
    receive
        {Cell, done} ->
            gol:wait_cell_update(From, AllCells, UpdateCounter-1);
        Any ->
            io:format("wait loop got unknown message: ~p~n", [Any]),
            gol:wait_cell_update(From, AllCells, UpdateCounter)
    end.
    


