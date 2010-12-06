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
    return_json(json:encode(json_struct([{pid, PidStr}])));

handle_start(_, _) ->
    gol_rpc_server:error_message(400, "start requires initialstate").

%% tick game

handle_tick(_, [{"pid", PidStr}]) ->
    % This is a quick "hack" since I dodn't had the time to check how to 
    % properly encode < and > into the url.
    MyPid = "<" ++ PidStr ++ ">",
    % this can fail and should be handled. 
    % the supplied string might be in the wrong format
    % the pid might have died
    Pid = list_to_pid(MyPid),
    gol:tick(Pid),
    Res = gol:get_state(Pid),
    Json = json:encode(json_array(Res)),
    % the current state of the game is returned as json
    return_json(Json);

handle_tick(_, _) ->
    gol_rpc_server:error_message(400, "tick requires pid").

%% helpers

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
