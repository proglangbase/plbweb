%%
%%  plbdb interface as a module loaded with inets.
%%
-module(mod_plbdb).
-export([do/1]).

-record(mod,{init_data,
	     data=[],
	     socket_type=ip_comm,
	     socket,
	     config_db,
	     method,
	     absolute_uri=[],
	     request_uri,
	     http_version,
	     request_line,
	     parsed_header=[],
	     entity_body,
	     connection}).

do(#mod{method="GET", request_uri="/favicon.ico"} = Info) -> 
  {proceed, [{status, {204, Info#mod.request_uri, "Not Found"}}]};

do(Info) -> 
  case whereis(plbdb_single) of
    Pid when is_pid(Pid) -> call_plbdb_html(Pid, Info#mod.request_uri);
    _ -> break_response(200, html([]))
  end.

break_response(Code,Body) -> {break,[{response,{Code,Body}}]}.

call_plbdb_html(Pid, Route) ->
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

html(_) ->
  "<!DOCTYPE html>"
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
  .
