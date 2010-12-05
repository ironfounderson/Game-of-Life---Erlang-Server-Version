-module(gol_rpc_server).
-compile(export_all).

-include("/Users/robert/code/erlang/yaws/include/yaws_api.hrl").

-import(gol).

box(Str) ->
    {'div',[{class,"box"}],
     {pre,[],Str}}.

out3(_) ->
    J = json:encode({struct, [{key1, "value1"}, {key2, "value2"}]}),
    return_json(J).

out(A) ->
    Path = lists:map(fun(X) -> string:to_lower(X) end, 
                     string:tokens(A#arg.appmoddata, "/")),
    handle_request(A, Path, A#arg.querydata).

    %% {ehtml,
    %%  [{p,[],
    %%    box(io_lib:format("appmoddata = ~p~n"
    %%                      "method  = ~p~n"
    %%                      "querydata = ~p~n",
    %%                      [A#arg.appmoddata,
    %%                       A#arg.req#http_request.method,
    %%                       A#arg.querydata]))}]}.

handle_request(A, ["start"], undefined) ->
    {ehtml,
     [{p,[],
       box(io_lib:format("Unknown stuff~p~n", [""]))}]};

handle_request(A, ["start"], Querydata) ->
    Parameters = [string:tokens(P, "=")|| P  <- string:tokens(Querydata, "&")],
    KeyValues = [{string:to_lower(Key), Value} || [Key, Value] <- Parameters],
    io:format("p=~p~n", [KeyValues]),
    handle_start(A, KeyValues);

handle_request(A, ["tick"], Querydata) ->
    Parameters = [string:tokens(P, "=")|| P  <- string:tokens(Querydata, "&")],
    KeyValues = [{string:to_lower(Key), Value} || [Key, Value] <- Parameters],
    io:format("p=~p~n", [KeyValues]),
    handle_tick(A, KeyValues).
    
    %H = A#arg.headers,
    %C = H#headers.cookie,
    %PidStr = yaws_api:find_cookie_val("game", C),
    %io:format("~p~n", [PidStr]),
    
    %Pid = list_to_pid(PidStr),
    %io:format("will tick ~n"),
    %gol:tick(Pid),
    %io:format("did tick").

    %% State = gol:get_state(game),
    %% Json = json:encode(State),
    %% return_json(Json).

handle_tick(A, [{"pid", PidStr}]) ->
    MyPid = "<" ++ PidStr ++ ">",
    Pid = list_to_pid(MyPid),
    gol:tick(Pid),
    Res = gol:get_state(Pid),
    io:format("~p~n", [Res]),
    Json = encode:json(Res),
    return_json(Json).

handle_start(A, [{"initialstate", InitialState}]) ->
    StartState = string:tokens(InitialState, ";"),
    Pid = gol:start(StartState),
    PidStr = pid_to_list(Pid),
    %yaws_api:setcookie("game", PidStr),
    %H = A#arg.headers,
    %C = H#headers.cookie,
    %PidStr2 = yaws_api:find_cookie_val("game", C),
    io:format("~p~n", [PidStr]),
    return_json(json:encode(json_struct([{pid, PidStr}])));

handle_start(A, [{"rows", Rows}, {"cols", Cols}, {"initialstate", InitialState}]) ->
    io:format("Got rows = ~p, cols = ~p~n", [Rows,Cols]).

out2(A) ->
    
    ok.

json_array(L) ->
    {array, L}.

json_struct(X) ->
    {struct, X}.

return_json(Json) ->
    {content, 
    "application/json; charset=iso-8859-1", 
    Json}.



