%%
%%  Copyright © 2024 Christopher Augustus
%%
%%  This Source Code Form is subject to the terms of the Mozilla Public
%%  License, v. 2.0. If a copy of the MPL was not distributed with this
%%  file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%
-module(plbdb).
-export([html/1]).

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
