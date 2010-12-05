-module(gol_cell).

-compile(export_all).

%% cell stuff

cell(Row, Col, State, Neighbours) ->
    {row, Row, col, Col, state, State, neighbours, Neighbours}.

row({row, Row, col, _, state, _, neighbours, _}) ->
    Row.

col({row, _, col, Col, state, _, neighbours, _}) ->
    Col.

state({row, _, col, _, state, State, neighbours, _}) ->
    State.

neighbours({row, _, col, _, state, _, neighbours, Neighbours}) ->
    Neighbours.

new_state(living, LivingNeighbours) when LivingNeighbours < 2 ->
    dead;
new_state(living, LivingNeighbours) when LivingNeighbours > 3 ->
    dead;
new_state(living, _)  ->
    living;
new_state(dead, 3) ->
    living;
new_state(_, _) ->
    dead.

update_state({row, Row, col, Col, state, State, neighbours, Neighbours}, LivingNeighbours) ->
    gol_cell:cell(Row, Col, gol_cell:new_state(State, LivingNeighbours), Neighbours).

init_loop(Row, Col, State) ->
    io:format("Cell ~p;~p created with state = ~p~n", [Row, Col, State]),
    receive
        {set_neighbours, Neighbours} ->
            gol_cell:loop(cell(Row, Col, State, Neighbours), none)
    end.

loop(Cell, Game) ->
    receive
        {From, tick} ->
            io:format("~p:~p got tick~n", [gol_cell:row(Cell), gol_cell:col(Cell)]),
            [C ! {state, state(Cell)} || C <- neighbours(Cell)],
            gol_cell:wait_update(Cell, From, 0, length(neighbours(Cell)));
        {_, exit} ->
            io:format("~p:~p got exit~n", [gol_cell:row(Cell), gol_cell:col(Cell)]);
        Any ->
            io:format("any message: ~p~n", [Any]),
            gol_cell:loop(Cell, Game)
    end.


wait_update(Cell, Game, LiveCounter, 0) ->
    io:format("~p;~p has ~p live neighbours~n", [row(Cell), col(Cell), LiveCounter]),
    NewCell = update_state(Cell, LiveCounter),
    Game ! {self(), done},
    gol_cell:loop(NewCell, Game);

wait_update(Cell, Game, LiveCounter, TotalCounter) ->
    receive
        {state, living } ->
            gol_cell:wait_update(Cell, Game, LiveCounter + 1, TotalCounter - 1); 
        {state, dead} ->
            gol_cell:wait_update(Cell, Game, LiveCounter, TotalCounter - 1);
        Any  ->
            io:format("got: ~p~n", [Any])
    end.
