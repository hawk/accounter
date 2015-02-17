%% Copyright 2015 Hakan Mattsson
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.

-module(accounter_app).
-compile(export_all).

-behaviour(application).
-export([start/2, stop/1]).

%% start

start(_Type, _StartArgs) ->
    accounter_sup:start_link().

%% stop

stop(_State) ->
    ok.
