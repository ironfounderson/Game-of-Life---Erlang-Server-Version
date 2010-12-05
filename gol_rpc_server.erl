-module(gol_rpc_server).
-include("/Users/robert/code/erlang/yaws/include/yaws_api.hrl").
-compile(export_all).

box(Str) ->
    {'div',[{class,"box"}],
     {pre,[],Str}}.

out3(_) ->
    J = json:encode({struct, [{key1, "value1"}, {key2, "value2"}]}),
    return_json(J).

out(A) ->
    Path = lists:map(fun(X) -> string:to_lower(X) end, 
                     string:tokens(A#arg.appmoddata, "/")),
    handle_request(Path, A#arg.querydata).

    %% {ehtml,
    %%  [{p,[],
    %%    box(io_lib:format("appmoddata = ~p~n"
    %%                      "method  = ~p~n"
    %%                      "querydata = ~p~n",
    %%                      [A#arg.appmoddata,
    %%                       A#arg.req#http_request.method,
    %%                       A#arg.querydata]))}]}.

handle_request(["start"], undefined) ->
    {ehtml,
     [{p,[],
       box(io_lib:format("Unknown stuff~p~n", [""]))}]};

handle_request(["start"], Querydata) ->
    Parameters = [string:tokens(P, "=")|| P  <- string:tokens(Querydata, "&")],
    KeyValues = [{string:to_lower(Key), Value} || [Key, Value] <- Parameters],
    io:format("p=~p~n", [KeyValues]),
    handle_start(KeyValues).

handle_start([{"rows", Rows}, {"cols", Cols}, {"initialstate", InitialState}]) ->
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



