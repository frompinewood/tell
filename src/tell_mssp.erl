-module(tell_mssp).
-include("tell.hrl").
-export([format/1]).
%% Mud Server Status Protocol 

-define(MSSP, 70).
-define(MSSP_VAR, 1).
-define(MSSP_VAL, 2).

format(Data) when is_map(Data) ->
  Str = maps:fold(fun (Key, Val, Acc) -> 
                Acc ++ [?MSSP_VAR, Key, ?MSSP_VAL, Val]    
            end, [], Data),
  [?IAC, ?SB, ?MSSP, Str, ?IAC, ?SE].
