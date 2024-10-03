%% 
%% Module passed into `erl_call` to start plbweb node, if not running, and
%% run plbinets web server.
%%
-module(plbweb_run).
-export([start/0]).
start() -> ok = file:set_cwd("code/erlang"), 
make:all(), 
c:l(plbweb),
plbinets:acquire(), 
ok = file:set_cwd("../..").
