-module(tell_nvt).

%% @doc
%% This module offers an NVT interface as described in RFC 854.
%% The purpose of the module is to allow the programmer to parse
%% individual lines of telnet commands from a buffer allowing half-
%% duplex functionality when required. Such as a client proposing
%% an option and then additional data intended to be parsed with that option in mind.
%% @end

-record(nvt, {buffer, state}).
-type nvt() :: {nvt, {buffer, state}}.

-export([nvt/0, nvt_push/2, nvt_pull/1, state/1]).

-spec nvt() -> nvt().
nvt() -> #nvt{buffer = queue:new(), state = maps:new()}.

-spec nvt_push(string(), nvt()) -> nvt().
nvt_push(Line, #nvt{buffer = Buffer, state = State} = Nvt) ->
    {Tel, Message} = tell:parse(Line),
    Nvt#nvt{
        state = maps:merge(maps:from_list(Tel), State),
        buffer =
            queue:join(
                Buffer,
                queue:from_list(
                  lists:map(
                    fun string:trim/1, 
                    string:split(Message, "\n", all)))
            )
    }.

-spec nvt_pull(nvt()) -> {string() | empty, nvt()}.
nvt_pull(#nvt{buffer = Buffer} = Nvt) ->
    {Value, NewBuffer} = queue:out(Buffer),
    Line =
        case Value of
            empty -> empty;
            {value, Data} -> Data
        end,
    {Line, Nvt#nvt{buffer = NewBuffer}}.

-spec state(nvt()) -> map().
state(Nvt) -> Nvt#nvt.state.
