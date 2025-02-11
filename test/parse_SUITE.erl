-module(parse_SUITE).
-include_lib("stdlib/include/assert.hrl").
-export([all/0]).
-export([iac_escape/1, iac_negotiate/1, iac_sub_negotiate/1]).

all() ->
    [iac_escape, iac_negotiate, iac_sub_negotiate].

iac_escape(_Config) ->
    {[], [255]} = tell:parse([255, 255]).

iac_negotiate(_Config) ->
    ?assertEqual({[{will, echo}], []}, tell:parse([255, 251, 1])),
    ?assertEqual({[{wont, echo}], []}, tell:parse([255, 252, 1])),
    ?assertEqual({[{do, echo}], []}, tell:parse([255, 253, 1])),
    ?assertEqual({[{dont, echo}], []}, tell:parse([255, 254, 1])).

iac_sub_negotiate(_Config) ->
  ?assertEqual({[{neg, [1, 255]}], "Hello world!"}, 
      tell:parse("Hello "++[255, 250, 1, 255, 255, 255, 240]++"world!")).
