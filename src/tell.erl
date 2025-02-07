-module(tell).

-include("tell.hrl").

-export([parse/1]).

-spec parse(Data::iodata()) -> {list(tuple()), iodata()}.
parse(Data) ->
  parse(Data, [], []).

parse([], Terms, Data) -> {lists:reverse(Terms, lists:reverse(Data))};
parse([?IAC|Rest], Terms, Data) ->
  parse_iac(Rest, Terms, Data);
parse([Byte|Rest], Terms, Data) ->
  parse(Rest, Terms, [Byte|Data]).

%%% IAC escaped
parse_iac([?IAC|Rest], Terms, Data) ->
  parse(Rest, Terms, [?IAC | Data]);
parse_iac([Command, Option|Rest], Terms, Data) 
  when Command =:= ?WILL orelse 
       Command =:= ?WONT orelse 
       Command =:= ?DO orelse 
       Command =:= ?DONT ->
    parse(Rest, [{command_to_atom(Command), option_to_atom(Option)}|Terms], Data).

command_to_atom(_) -> undefined.
option_to_atom(_) -> undefined.
  



