-module(gol).
-compile(export_all).

%% initial state is represented by an array of strings where 0 = empty cell
%% and 1 = occupied cell

start_blinker() ->
    start(["00000", "00000", "01110", "00000", "00000"]).

start([IS | InitialStates]) ->
    io:format("Columns = ~p, Rows = ~p~n", 
              [length(IS), 
               length([IS|InitialStates])]),
    ok.

