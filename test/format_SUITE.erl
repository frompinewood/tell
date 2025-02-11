-module(format_SUITE).

-export([all/0]).
-export([format_command/1, format_command_option/1, format_list/1]).

-include_lib("stdlib/include/assert.hrl").
-include("tell.hrl").

all() ->
  [format_command, format_command_option, format_list].

format_command(_Config) ->
  ?assertEqual(tell:format({will, echo}), [?IAC, ?WILL, ?ECHO]).

format_command_option(_Config) ->
  ?assertEqual(tell:format(are_you_there), [?IAC, ?AYT]).

format_list(_Config) ->
  ?assertEqual(tell:format([{wont, binary}, are_you_there]), [[?IAC, ?WONT, ?BINARY], [?IAC, ?AYT]]).
