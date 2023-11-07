-module(plbinets).
-export([acquire/0, init/0]).
-define(PROCESS, list_to_atom(atom_to_list(?MODULE) ++ "_single")).

acquire() ->
  case whereis(?PROCESS) of
    Pid when is_pid(Pid) -> Pid;
    _ -> spawn(?MODULE, init, [])
  end.

init() ->
  ok = application:ensure_started(inets),
  {ok,PidHttpd} = inets:start(httpd, [
    {server_name,   "plbweb"    },
    {server_root,   "."         },
    {document_root, "."         },
    {bind_address,  "localhost" },
    {port,          8088        }
  ]),
  register(?PROCESS, self()),
  receive
    stop -> inets:stop(httpd, PidHttpd), ok;
    stop_inets -> inets:stop(), ok
  end.
