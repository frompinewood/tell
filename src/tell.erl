-module(tell).

-include("tell.hrl").

-export([parse/1, 
         atom_to_command/1, 
         atom_to_option/1, 
         option_to_atom/1, 
         command_to_atom/1]).

%% expand type to show supported atoms
-type iac_command() :: atom() |
                       {atom(), atom()} |
                       {atom(), iodata()}.

-spec parse(Data :: iodata()) -> {list(iac_command()), iodata()}.
parse(Data) ->
    parse(Data, [], []).

parse([], Terms, Data) -> {lists:reverse(Terms), lists:reverse(Data)};
parse([?IAC | Rest], Terms, Data) -> parse_iac(Rest, Terms, Data);
parse([Byte | Rest], Terms, Data) -> parse(Rest, Terms, [Byte | Data]).

%%% IAC EOD
parse_iac([], Terms, Data) ->
    parse([], [invalid | Terms], Data);
%%% IAC escaped
parse_iac([?IAC | Rest], Terms, Data) ->
    parse(Rest, Terms, [?IAC | Data]);
%% Begin negotation
parse_iac([?SB | Rest], Terms, Data) ->
    parse_sub_neg(Rest, Terms, Data, []);
%%% Command >= 251 and <= 254
parse_iac([Command, Option | Rest], Terms, Data) 
    when Command =< ?DONT andalso 
         Command >= ?WILL ->
      parse(Rest, [{command_to_atom(Command), option_to_atom(Option)} | Terms], Data);
parse_iac([Command|Rest], Terms, Data) ->
    parse(Rest, [command_to_atom(Command)|Terms], Data).

%% @todo handle nested IAC within a subnegotiation
%% Terminating clause - subnegotiation end
parse_sub_neg([?IAC,?SE|Rest], Terms, Data, Neg) ->
    parse(Rest, [{neg, lists:reverse(Neg)} | Terms], Data);
%% Escape IAC
parse_sub_neg([?IAC,?IAC|Rest], Terms, Data, Neg) ->
    parse_sub_neg(Rest, Terms, Data, [?IAC | Neg]);
%% Append negotiation
parse_sub_neg([Byte|Rest], Terms, Data, Neg) ->
  parse_sub_neg(Rest, Terms, Data, [Byte|Neg]).

command_to_atom(?NOP) -> no_op;
command_to_atom(?DM) -> data_mark;
command_to_atom(?BRK) -> break;
command_to_atom(?IP) -> interrupt_process;
command_to_atom(?GA) -> go_ahead;
command_to_atom(?AYT) -> are_you_there;
command_to_atom(?EL) -> erase_line;
command_to_atom(?EC) -> erase_char;
command_to_atom(?AO) -> abort_output;
command_to_atom(?WILL) -> will;
command_to_atom(?WONT) -> wont;
command_to_atom(?DO) -> do;
command_to_atom(?DONT) -> dont;
command_to_atom(Command)  
  when is_integer(Command) andalso 
       Command =< 256 andalso 
       Command >= 0 -> {unknown, Command}.

option_to_atom(?ECHO) -> echo;
option_to_atom(?STATUS) -> status;
option_to_atom(?BINARY) -> binary;
option_to_atom(?SUPPRESS_GO_AHEAD) -> suppress_go_ahead;
option_to_atom(?NEW_ENVIRON) -> new_environ;
option_to_atom(Option)
  when is_integer(Option) andalso 
       Option =< 256 andalso 
       Option >= 0 -> {unknown, Option}.

atom_to_command(no_op) -> ?NOP;
atom_to_command(data_mark) -> ?DM;
atom_to_command(break) -> ?BRK;
atom_to_command(interrupt_process) -> ?IP;
atom_to_command(go_ahead) -> ?GA;
atom_to_command(are_you_there) -> ?AYT;
atom_to_command(erase_line) -> ?EL;
atom_to_command(erase_char) -> ?EC;
atom_to_command(abort_output) -> ?AO;
atom_to_command(will) -> ?WILL;
atom_to_command(wont) -> ?WONT;
atom_to_command(do) -> ?DO;
atom_to_command(dont) -> ?DONT.

atom_to_option(echo) -> ?ECHO;
atom_to_option(status) -> ?STATUS;
atom_to_option(binary) -> ?BINARY;
atom_to_option(suppress_go_ahead) -> ?SUPPRESS_GO_AHEAD;
atom_to_option(new_environ) -> ?NEW_ENVIRON.
