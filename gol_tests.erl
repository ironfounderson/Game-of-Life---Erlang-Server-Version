-module(gol_tests).

-include_lib("eunit/include/eunit.hrl").

simple_test() ->
    ?assert(1 == 1).

is_neighbour(Row, Col, Row, Col) -> 
    false;

is_neighbour(Row, Col, NeighbourRow, NeighbourCol) ->
    (abs(Row - NeighbourRow) =< 1) and (abs(Col - NeighbourCol) =< 1).

is_neighbour1_test() ->
    ?assert(is_neighbour(0, 0, 0, 1)).
is_neighbour2_test() ->
    ?assert(not is_neighbour(0,0,0,0)).
is_neighbour3_test() ->
    ?assert(not is_neighbour(0,0, 0, 2)).
                                                                          
cell(Pid, Row, Col) ->
    {pid, Pid, row, Row, col, Col}.

neighbour(Row, Col, AllCells) ->
    [Pid || {pid, Pid, _, _, _, _ } 
                <- lists:filter(
                     fun({_, _, row, R, col, C}) -> 
                             is_neighbour(Row, Col, R, C) 
                     end, 
                     AllCells)].
    

neighbour_test() ->
    AllCells = [cell("cell00",0,0), cell("cell01", 0, 1), cell("cell10", 1, 0), cell("cell11", 1, 1)],
    Expected = ["cell01", "cell10", "cell11"],
    ?assertMatch(Expected,
                 neighbour(0, 0, AllCells)).
    

