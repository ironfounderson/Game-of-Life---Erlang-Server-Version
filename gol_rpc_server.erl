-module(gol_rpc_server).
-compile(export_all).

-include("/Users/robert/code/erlang/yaws/include/yaws_api.hrl").

-import(gol).

box(Str) ->
    {'div',[{class,"box"}],
     {pre,[],Str}}.

%% We start by switching on the path supplied to us
%% For the game of life server we have implemented start and tick

out(A) ->
    Path = lists:map(fun(X) -> string:to_lower(X) end, 
                     string:tokens(A#arg.appmoddata, "/")),
    gol_rpc_server:route_request(A, Path, A#arg.querydata).

%% {ehtml,
%%  [{p,[],
%%    box(io_lib:format("appmoddata = ~p~n"
%%                      "method  = ~p~n"
%%                      "querydata = ~p~n",
%%                      [A#arg.appmoddata,
%%                       A#arg.req#http_request.method,
%%                       A#arg.querydata]))}]}.


%% route our request to the correct method

route_request(A, ["start"], Querydata) ->
    handle_start(A, gol_rpc_server:query_to_keyvalues(Querydata));

route_request(A, ["tick"], Querydata) ->
    handle_tick(A, gol_rpc_server:query_to_keyvalues(Querydata));

route_request(A, ["exit"], Querydata) ->
    handle_exit(A, gol_rpc_server:query_to_keyvalues(Querydata));

route_request(_, Path, _) ->
    io:format("Server got unknown path:~p~n", [Path]),
    gol_rpc_server:error_message(400, "Unknown path").

%% start new game

handle_start(_, [{"initialstate", InitialState}]) ->
    % initial state is given as a string of 0's and 1's where each row is
    % separated by ;
    % Note: No check is done wether the string is valid
    StartState = string:tokens(InitialState, ";"),
    Pid = gol:start(StartState),
    PidStr = pid_to_list(Pid),
    % we return the Pid which must be supplied in the calls to tick
    % Note: we probably should keep some kind of dictionary and return
    % an unique key that is mapped to the pid but this is just for testing
    io:format("got start request with initial state:~p~n", [InitialState]),
    return_json(json:encode(json_struct([{pid, PidStr}])));
handle_start(_, _) ->
    gol_rpc_server:error_message(400, "start requires initialstate").

%% tick game

handle_tick(_, [{"pid", PidStr}]) ->
    case gol_rpc_server:get_process(PidStr) of
        undefined -> 
            gol_rpc_server:error_message(400, "unknown pid");
        Pid ->
            io:format("got tick request for ~p~n", [Pid]),
            gol:tick(Pid),
            Res = gol:get_state(Pid),
            io:format("returning response~n"),
            Json = json:encode(json_array(Res)),
            return_json(Json)
    end;
handle_tick(_, _) ->
    gol_rpc_server:error_message(400, "tick requires pid").


%% tick game

handle_exit(_, [{"pid", PidStr}]) ->
    case gol_rpc_server:get_process(PidStr) of
        undefined -> 
            gol_rpc_server:error_message(400, "unknown pid");
        Pid ->
            io:format("got exit request for ~p~n", [Pid]),
            gol:exit(Pid),
            gol_rpc_server:error_message(200, "OK")
    end;
handle_exit(_, _) ->
    gol_rpc_server:error_message(400, "exit requires pid").

%% helpers

% returns undefined if PidStr is not in the ocrrect format or if the process
% no longer exists
get_process(PidStr) ->
    % This is a quick "hack" since I didn't had the time to check how to 
    % properly encode < and > into the url.
    MyPid = "<" ++ PidStr ++ ">",
    try list_to_pid(MyPid) of
        Pid -> 
           case erlang:process_info(Pid) of
               undefined -> undefined;
               _ -> Pid
           end
    catch
        _:_ ->
            undefined
    end.

error_message(Code, Message) ->
    [{status, Code},
     {content, "text/plain", Message}].

query_to_keyvalues(undefined) ->
    [];

query_to_keyvalues(Querydata) ->
    % First split the query string into parameters by & and then into key value pair
    % by splitting on =
    % Note: this will not work if parameters are suppliedin another form than 
    % key=value
    Parameters = [string:tokens(P, "=")|| P  <- string:tokens(Querydata, "&")],
    [{string:to_lower(Key), Value} || [Key, Value] <- Parameters].
    

%% json helper methods

json_array(L) ->
    {array, L}.

json_struct(X) ->
    {struct, X}.

return_json(Json) ->
    {content, 
     "application/json; charset=iso-8859-1", 
     Json}.
