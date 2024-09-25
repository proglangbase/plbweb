%%
%%  Copyright © 2023 - 2024 Christopher Augustus
%%
%%  This Source Code Form is subject to the terms of the Mozilla Public
%%  License, v. 2.0. If a copy of the MPL was not distributed with this
%%  file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%
-module(plbinets).
-export([acquire/0, create/1, do/1, init/1, start/2]).

-define(MODULE_PATH         , filename:dirname(proplists:get_value(source, module_info(compile)))).
-define(NAME_SINGLETON      , list_to_atom(?MODULE_STRING++"_single")).
-define(HTTPD_ADDR_LOCAL    , "localhost"                 ).
-define(HTTPD_PORT_LOCAL    , 8088                        ).
-define(HTTPD_PORT_PUBLIC   , 4433                        ).
-define(HTTPD_SERVER_ROOT   , ?MODULE_PATH++"/../.."      ).
-define(HTTPD_DOCUMENT_ROOT , ?MODULE_PATH++"/."          ).
-define(PATH_CERT           , ?HTTPD_SERVER_ROOT++"/cert" ).
-define(PATH_LOG            , "log"                       ).
-define(FILE_CERT           , ?PATH_CERT++"/fullchain.pem").
-define(FILE_KEY            , ?PATH_CERT++"/privkey.pem"  ).

acquire() ->
  case whereis(?NAME_SINGLETON) of
    Pid when is_pid(Pid) -> Pid;
    _ -> create(local)
  end.

create(Addr) ->
  case whereis(?NAME_SINGLETON) of
    Pid when is_pid(Pid) -> {error,already_exists_at_pid,Pid};
    _ -> spawn(?MODULE, init, [Addr])
  end.

init(Addr) ->
  case Addr of
    local -> start(?HTTPD_ADDR_LOCAL, ?HTTPD_PORT_LOCAL );
    _     -> start(Addr             , ?HTTPD_PORT_PUBLIC)
  end.

start(Addr, Port) ->
  case {file:read_file_info(?FILE_CERT),
        file:read_file_info(?FILE_KEY )} of
      {{ok,_},{ok,_}} -> %% support HTTPS
        ok = application:ensure_started(asn1),
        ok = application:ensure_started(crypto),
        ok = application:ensure_started(public_key),
        ok = application:ensure_started(ssl),
        SocketType = {socket_type,
          {ssl,[{certfile,?FILE_CERT},{keyfile,?FILE_KEY}]}};
      _ -> SocketType = {ip_comm},
          report("No HTTPS; missing cert files")
  end,
  DirLog = ?HTTPD_SERVER_ROOT++"/"++?PATH_LOG,
  case file:make_dir(DirLog) of
    ok -> report("Made directory "++DirLog), ok;
    {error,eexist} -> ok;
    Bad -> ok = Bad
  end,
  ok = application:ensure_started(inets),
  {ok,PidHttpd} = inets:start(httpd, [
     {bind_address  ,Addr                       }
    ,{document_root ,?HTTPD_DOCUMENT_ROOT       }
    ,{port          ,Port                       }
    ,{server_name   ,Addr                       }
    ,{server_root   ,?HTTPD_SERVER_ROOT         }
    ,SocketType
    ,{modules       ,[mod_log, plbinets]        }
    %% mod_log config:
    ,{error_log     ,?PATH_LOG++"/error.log"    }
    ,{security_log  ,?PATH_LOG++"/security.log" }
    ,{transfer_log  ,?PATH_LOG++"/transfer.log" }
  ]),
  register(?NAME_SINGLETON, self()),
  receive
    stop        -> inets:stop(httpd, PidHttpd), ok;
    stop_inets  -> inets:stop(), ok
  end.

do(ModData) -> %% {proceed, OldData} | {proceed, NewData} | {break, NewData} | done
  Route = element(9, ModData),
  %%io:fwrite("Route: ~p~n",[Route]),
  %%io:fwrite("ModData: ~p~n",[ModData]),
  case whereis(plbdb_single) of
    Pid when is_pid(Pid) -> call_plbdb_html(Pid, Route);
    _ -> break_response(200, plbdb:html(Route))
  end.

break_response(Code,Body) -> {break,[{response,{Code,Body}}]}.

call_plbdb_html(Pid, Route) ->
  %%io:fwrite("calling plbdb with Route: ~p~n",[Route]),
  Ref = erlang:monitor(process, Pid),
  Pid ! {html,Route,Ref,self()},
  receive
    {html,Html,Ref,Pid} ->
      erlang:demonitor(Ref, [flush]),
      break_response(200, Html);
    {error_unknown_message,Msg} ->
      erlang:demonitor(Ref, [flush]), % 502 Bad Gateway
      break_response(502, io_lib:format("sent unknown message: ~p~n", [Msg]));
    {'DOWN',Ref,process,Pid,Reason} -> % 503 Service Unavailable
      break_response(503, io_lib:format("data service crashed: ~p~n", [Reason]));
    Msg -> io:fwrite("unknown message: ~p~n", [Msg])
  after 1000 ->
    erlang:demonitor(Ref, [flush]), % 504 Gateway Timeout
    break_response(504, "timed out waiting for data")
  end.

report(Info) ->
  %% TODO: ### ALSO OUTPUT TO LOG FILE
  io:fwrite(Info++".\n").
