-module(emacs). 
-author("sean dot charles at objitsu dot com"). 

%% Change to match your erlang installation 
-include("/Users/robert/code/erlang/yaws/include/yaws_api.hrl"). 
-export([out/1]). 

%% ------------------------------------------------------------ 
%% This causes yaws to compile and reload the named module. 
%% A corresponding emacs hook will make this call every time we 
%% C-x C-s in a buffer ending with .erl. 
%% 
%% This should provide for a 'seamless' integration between the 
%% emacs editor and the yaws application being developed and 
%% make life a whole lot easier for developers everywhere. 
%% 
%% Assumes that the emacs argument is the ABSOLUTE file path 
%% and uses that information to: 
%% 
%% 1)  cd("the containing directory"). 
%% 2)  c("the filename"). 
%% 
%% Because Erlang and yaws are things of beauty and elegance, 
%% this arrangement also provides the benefit of outputting all 
%% compiler errors to the yaws console as well which means that 
%% as soon as you save it you can see what went bang! 
%% 
%% This is 'almost' the beginning of a SLIME like IDE dare I 
%% say it: you *could* send the compiler output back as the 
%% return data from the request and display it emacs friendly 
%% format... maybe another day!  :) 
%%
%% You can run yaws from a shell inside emacs and have it in the 
%% bottom part of the display, that way you can see your errors 
%% as and when they happen. 
%% ------------------------------------------------------------ 

out(A) -> 
    Module  = filename:basename(A#arg.appmoddata), 
    % Mod_Dir = filename:dirname(A#arg.appmoddata), 

    %% Write a message to the yaws console... 
    io:format("!!! Emacs refreshed module: ~s~n", [Module]), 

    io:format("!!! CD: ~w~n", [c:cd("/Users/robert/code/erlang/gameoflife")]), 
    io:format("!!!  C: ~w~n", [c:c(Module)]), 
        ok.
