%%
%%  Copyright © 2024 Christopher Augustus
%%
%%  This Source Code Form is subject to the terms of the Mozilla Public
%%  License, v. 2.0. If a copy of the MPL was not distributed with this
%%  file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%
-module(erlsamelib).
-export([posteval/1]).

posteval(Source) ->
  io:fwrite("Executing arg posteval:~s~n", [Source]),
  try
    {ok, Tokens, _}  = erl_scan:string(Source),
    {ok, Exprs}      = erl_parse:parse_exprs(Tokens),
    io:fwrite("~p~n", [erl_eval:exprs(Exprs, [])])
  of
    _ -> ok
  catch
    _:Reason -> io:fwrite("FAILED:~n~p~n", [Reason])
  end.
