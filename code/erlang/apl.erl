%%
%%  Copyright © 2024 Christopher Augustus
%%
%%  This Source Code Form is subject to the terms of the Mozilla Public
%%  License, v. 2.0. If a copy of the MPL was not distributed with this
%%  file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%
-module(apl). %% note: this module name must match erlang_APL_nif.c
-export([init/0, print_keyboard/0, print_statement/1, test/0]).

% procedures defined in gnu-apl/erlang/erlang_APL_nif.c
-export([command_ucs/1, command_utf8/1, eval_mux/5, fix_function_ucs/1,
         set_variable/3, statement_ucs/1, statement_utf8/1]).

% required stubs overridden in erlang_APL_nif.c
apl_uninitialized()     -> exit('APL_uninitialized').
command_ucs(_)          -> apl_uninitialized().
command_utf8(_)         -> apl_uninitialized().
eval_mux(_, _, _, _, _) -> apl_uninitialized().
fix_function_ucs(_)     -> apl_uninitialized().
set_variable(_, _, _)   -> apl_uninitialized().
statement_ucs(_)        -> apl_uninitialized().
statement_utf8(_)       -> apl_uninitialized().

% convenience functions
rstr_or_len(S, T) -> case string:rstr(S, T) of 0 -> string:len(S); Pos -> Pos end.
trimr(S, T) -> string:left(S, rstr_or_len(S, T)).
%comb_s(Fa, Fb, C) -> Fa(C, Fb(C)).
%trimr(S, T) -> comb_s(fun string:left/2, fun (SS) -> rstr_or_len(SS, T) end, S).

init() ->
    AplSrcDir = string:chomp(os:cmd("apl --show_src_dir")),
    %%io:fwrite("## APL src dir: ~p~n", [AplSrcDir]),
    %% TODO: ### LD_LIBRARY_PATH MUST BE SET BEFORE BEAM IS LOADED, FOR libapl.so
    %%os:putenv("LD_LIBRARY_PATH", AplSrcDir ++ "/.libs"),
    AplDir = trimr(AplSrcDir, "/src"),
    erlang:load_nif(AplDir ++ "/erlang/erlang_APL_nif", 0).

print_keyboard() -> io:fwrite(
"╔════╦════╦════╦════╦════╦════╦════╦════╦════╦════╦════╦════╦════╦════════════╗\n" ++
% TODO: ### io:format/fwrite will not print a lone ~ as the doc says it will
%"║ ~  ║ !⌶ ║ @⍫ ║ #⍒ ║ $⍋ ║ %⌽ ║ ^⍉ ║ &⊖ ║ *⍟ ║ (⍱ ║ )⍲ ║ _! ║ +⌹ ║            ║\n" ++
"║    ║ !⌶ ║ @⍫ ║ #⍒ ║ $⍋ ║ %⌽ ║ ^⍉ ║ &⊖ ║ *⍟ ║ (⍱ ║ )⍲ ║ _! ║ +⌹ ║            ║\n" ++
"║ `◊ ║ 1¨ ║ 2¯ ║ 3< ║ 4≤ ║ 5= ║ 6≥ ║ 7> ║ 8≠ ║ 9∨ ║ 0∧ ║ -× ║ =÷ ║ BACKSP     ║\n" ++
"╠════╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═════════╣\n" ++
"║       ║ Q  ║ W⍹ ║ E⍷ ║ R  ║ T⍨ ║ Y¥ ║ U€ ║ I⍸ ║ O⍥ ║ P⍣ ║ {⍞ ║ }⍬ ║         ║\n" ++
"║  TAB  ║ q? ║ w⍵ ║ e∈ ║ r⍴ ║ t∼ ║ y↑ ║ u↓ ║ i⍳ ║ o○ ║ p⋆ ║ [← ║ ]→ ║ RETURN  ║\n" ++
"╠═══════╩═╦══╩═╦══╩═╦══╩═╦══╩═╦══╩═╦══╩═╦══╩═╦══╩═╦══╩═╦══╩═╦══╩═╦══╩═╗       ║\n" ++
"║ (CAPS   ║ A⍶ ║ S  ║ D  ║ F  ║ G  ║ H  ║ J⍤ ║ K⌸ ║ L⌷ ║ :≡ ║ \"≢ ║ |⊣ ║       ║\n" ++
"║  LOCK)  ║ a⍺ ║ s⌈ ║ d⌊ ║ f_ ║ g∇ ║ h∆ ║ j∘ ║ k' ║ l⎕ ║ ;⍎ ║ '⍕ ║ \\⊢ ║       ║\n" ++
"╠════════╦╩═══╦╩═══╦╩═══╦╩═══╦╩═══╦╩═══╦╩═══╦╩═══╦╩═══╦╩═══╦╩═══╦╩════╩═══════╣\n" ++
"║        ║ < ¦║ Z  ║ Xχ ║ C¢ ║ V  ║ B£ ║ N  ║ M  ║ <⍪ ║ >⍙ ║ ?⍠ ║             ║\n" ++
"║  SHIFT ║ > °║ z⊂ ║ x⊃ ║ c∩ ║ v∪ ║ b⊥ ║ n⊤ ║ m| ║ ,⍝ ║ .⍀ ║ /⌿ ║  SHIFT      ║\n" ++
"╚════════╩════╩════╩════╩════╩════╩════╩════╩════╩════╩════╩════╩═════════════╝").

print_statement(S) ->
    statement_ucs("⎕←'<-- " ++ S ++ "' ◊ 8 ⎕CR " ++ S).

test() ->
    print_statement("⍳9"),
    print_statement("+\\⍳9"),
    print_statement("!¨⍳9"),
    print_statement("3 3⍴⍳9"),
    print_statement("+\\3 3⍴⍳9"),
    print_statement("!¨3 3⍴⍳9"),
    print_statement("3 3 3⍴⍳27"),
    print_statement("+\\3 3 3⍴⍳27"),
    print_statement("!¨3 3 3⍴⍳27").
