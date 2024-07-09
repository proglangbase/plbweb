%%
%%  Copyright © 2024 Christopher Augustus
%%
%%  This Source Code Form is subject to the terms of the Mozilla Public
%%  License, v. 2.0. If a copy of the MPL was not distributed with this
%%  file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%
-module(plbdb).
-export([home/0, init/0]).

init() ->
  ok = apl:init().

html_table_gnuapl() ->
  %%"∇Z ← HtmlTableRows Y;tds;trs\n"
  %%"tds ← {'<td>',⍵,'</td>'}¨¨Y\n"
  %%"trs ← {(⊂'<tr>'),⍵,(⊂'</tr>')}¨tds\n"
  %%"Z ← trs\n"
  %%"∇\n"
  %%"∇Z ← PlbDataRaw\n"
  "rows ← \""
  ",lang     ,para"
  ",APL      ,a"
  ",BQN      ,a"
  ",C        ,p"
  ",Erlang   ,f"
  ",J        ,a"
  "\""
  " ◊ rows"
  %%"Z ← {(≢⍵) 1⍴⍵}{({(+\¨⍵)×(1-⍵)}','∈⍨¨⍵)⊂¨⍵}rows\n"
  %%"∇\n"
  %%"HtmlTableRows PlbDataRaw\n"
  %%"\"<td>APL TABLE CELL</td>\""
.

home() ->
  "<!DOCtYPE html>"
  "<html>"
  "<head>"
  "  <title>proglangbase</title>"
  "</head>"
  "<body>"
  "  <h1>proglangbase</h1>"
  "  <table>"
  ++
  apl:statement_into_string(html_table_gnuapl())
  %%html_table_gnuapl()
  ++
  "  </table>"
  "</body>"
  "</html>"
.
