-module(gol).
-compile(export_all).

-import(gol_cell).

%% initial state is represented by an array of strings where 0 = empty cell
%% and 1 = occupied cell

start_blinker() ->
    start(["00000", "00000", "01110", "00000", "00000"]).

start_test() ->
    start(["000", "111", "000"]).


start(InitialStates) ->
%    spawn(fun() -> gol:init([IS | InitialStates]) end),

    AllCells = gol:generate_cells(lists:reverse(InitialStates)),
    
    gol:assign_neighbours(AllCells, AllCells),
    spawn (fun() -> gol:loop(AllCells) end).

%% start([IS | InitialStates]) ->
%%     % Spawn main game process so I can link to the cell processes
%%     spawn(fun() -> gol:init([IS | InitialStates]) end).

%% init([IS | InitialStates]) ->
%%     AllCells = generate_cells(length(IS),
%%                               length([IS|InitialStates]),
%%                               lists:reverse([IS|InitialStates])),
%%     gol:assign_neighbours(AllCells, AllCells),
%%     gol:loop(AllCells).
    

generate_cells([]) ->
     [];
generate_cells([IS | InitialStates]) ->
    Cells = gol:generate_column_cells(length([IS|InitialStates]), 
                                      lists:reverse(IS)),
    lists:append(Cells, gol:generate_cells(InitialStates)).

state(48) ->
    dead;
state(_) ->
    living.

generate_column_cells(_, []) ->
    [];
generate_column_cells(CurrentRow, [H|T]) ->
    CurrentCol = length([H|T]),
    CellPid = spawn_link(fun() -> gol_cell:init_loop(CurrentRow, CurrentCol, state(H)) end), 
    [{ pid, CellPid, row, CurrentRow, col, CurrentCol } |
     generate_column_cells(CurrentRow, T)].

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

tick() ->
    tick(game).

tick(Pid) ->
    rpc(Pid, tick).

exit(Pid) ->
    rpc(Pid, exit).

get_state() ->
    get_state(game).

get_state(Pid) ->
    rpc(Pid, get_state).

rpc(Pid, Request) ->
    Pid ! {self(), Request},
    receive
        {Pid, Response} ->
            Response
    end.


%% loop stuff

loop(AllCells) ->
    receive
        {From, tick} ->
            [Cell ! {self(), prepare} || {pid, Cell, _, _, _, _} <- AllCells ],
            gol:wait_cell_prepare(From, AllCells, length(AllCells));
        {From, get_state} ->
            [Cell ! {self(), get_state} || {pid, Cell, _, _, _, _} <- AllCells ],
            gol:wait_cell_state(From, AllCells, length(AllCells), []);            
        {From, exit} ->
            From ! {self(), "exiting"},
            erlang:exit(exit);
        Any ->
            io:format("main loop got unknown message: ~p~n", [Any]),
            gol:loop(AllCells)
    after
         % after 5 minutes the game will time out and die
        300000 ->
            io:format("exit on timeout~n"),
            erlang:exit(timeout)
    end.


%% waiting for all cells to acknowledge the prepare

wait_cell_prepare(From, AllCells, 0) ->
    [Cell ! {self(), tick} || {pid, Cell, _, _, _, _} <- AllCells ],
    gol:wait_cell_done(From, AllCells, length(AllCells));
wait_cell_prepare(From, AllCells, UpdateCounter) ->
    receive
        {_, prepare_done} ->
            gol:wait_cell_prepare(From, AllCells, UpdateCounter-1);
        Any ->
            io:format("prepare loop got unknown message: ~p~n", [Any]),
            gol:wait_cell_prepare(From, AllCells, UpdateCounter)
    end.
    

%% waiting for all cells to send an update

wait_cell_done(From, AllCells, 0) ->
    [Cell ! {self(), update} || {pid, Cell, _, _, _, _} <- AllCells ],
    gol:wait_cell_update(From, AllCells, length(AllCells));
wait_cell_done(From, AllCells, UpdateCounter) ->
    receive
        {_, cell_done} ->
            gol:wait_cell_done(From, AllCells, UpdateCounter-1);
        Any ->
            io:format("done loop got unknown message: ~p~n", [Any]),
            gol:wait_cell_done(From, AllCells, UpdateCounter)
    end.


%% waiting for all cells to send their current state
    
wait_cell_state(From, AllCells, 0, States) ->
    From ! {self(), States},
    gol:loop(AllCells);
wait_cell_state(From, AllCells, UpdateCounter, States) ->
    receive
        {Row, Col, State} ->
            CellData = {struct, [{row, Row}, {col, Col}, {state, atom_to_list(State)}]},
            gol:wait_cell_state(From, AllCells, UpdateCounter-1, 
                                [CellData|States])
    end.

wait_cell_update(From, AllCells, 0) ->
    From ! {self(), "Cell update done"},
    gol:loop(AllCells);
wait_cell_update(From, AllCells, UpdateCounter) ->
    receive
        {_, update_done} ->
            gol:wait_cell_update(From, AllCells, UpdateCounter-1)
    end.
