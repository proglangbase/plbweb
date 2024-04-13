%%
%%  Copyright © 2023 Christopher Augustus
%%
%%  This Source Code Form is subject to the terms of the Mozilla Public
%%  License, v. 2.0. If a copy of the MPL was not distributed with this
%%  file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%
-module(plbinets).
-export([acquire/0, do/1, init/0]).
-define(NAME_SINGLETON, list_to_atom(atom_to_list(?MODULE) ++ "_single")).

acquire() ->
    case whereis(?NAME_SINGLETON) of
        Pid when is_pid(Pid) -> Pid;
        _ -> spawn(?MODULE, init, [])
    end.

init() ->
    ok = application:ensure_started(inets),
    {ok, PidHttpd} = inets:start(httpd, [
        {server_name,     "proglangbase.org"    },
        {server_root,     "."                   },
        {document_root,   "."                   },
        {bind_address,    "localhost"           },
        {port,            8088                  },
        {modules,         [plbinets]            }
    ]),
    register(?NAME_SINGLETON, self()),
    receive
        stop        -> inets:stop(httpd, PidHttpd), ok;
        stop_inets  -> inets:stop(), ok
    end.

do(_) -> %% ModData) -> {proceed, OldData} | {proceed, NewData} | {break, NewData} | done
    {break, [{response, {200,
        "<!DOCtYPE html>"
        "<html>"
        "<head>"
        "  <title>proglangbase</title>"
        "</head>"
        "<body>"
        "  <h1>proglangbase</h1>"
        "  <table>"
        "    <tr><td>lang   </td><td>para </td></tr>"
        "    <tr><td>APL    </td><td>AP   </td></tr>"
        "    <tr><td>BQN    </td><td>AP   </td></tr>"
        "    <tr><td>C      </td><td>AP   </td></tr>"
        "    <tr><td>Erlang </td><td>AP   </td></tr>"
        "    <tr><td>F#     </td><td>FP   </td></tr>"
        "    <tr><td>J      </td><td>AP   </td></tr>"
        "    <tr><td>PHP    </td><td>PP   </td></tr>"
        "  </table>"
        "</body>"
        "</html>"
        }}]}.

