-module(gol_rpc_server).
-include("/Users/robert/code/erlang/yaws/include/yaws_api.hrl").
-compile(export_all).

box(Str) ->
    {'div',[{class,"box"}],
     {pre,[],Str}}.

out(A) ->
    {ehtml,
     [{p,[],
       box(io_lib:format("== appmoddata = ~p~n"
                         "method  = ~p~n"
                         "querydata = ~p~n",
                         [A#arg.appmoddata,
                          A#arg.req#http_request.method,
                          A#arg.querydata]))}]}.

