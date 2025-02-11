%% based on RFC 854

%% telnet commands
-define(SE, 240).
-define(NOP, 241).
-define(DM, 242).
-define(BRK, 243).
-define(IP, 244).
-define(AO, 245).
-define(AYT, 246).
-define(EC, 247).
-define(EL, 248).
-define(GA, 249).
-define(SB, 250).
-define(WILL, 251).
-define(WONT, 252).
-define(DO, 253).
-define(DONT, 254).
-define(IAC, 255).

%% telnet options
%% RFC 856
-define(BINARY, 0).
%% RFC 857
-define(ECHO, 1).
%% RFC 858
-define(SUPRESS_GO_AHEAD, 3).
%% RFC 859
-define(STATUS, 5).
%% RFC 1572
-define(NEW_ENVIRON, 39).

%% Subnegotiation Terms
-define(IS, 0).
-define(SEND, 1).
-define(INFO, 2).

%% New Environ Terms 

-define(VAR, 0).
-define(VALUE, 1).
-define(ESC, 2).
-define(USERVAR, 3).

