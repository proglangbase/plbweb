⍝
⍝   Copyright © 2024 Christopher Augustus
⍝
⍝   This Source Code Form is subject to the terms of the Mozilla Public
⍝   License, v. 2.0. If a copy of the MPL was not distributed with this
⍝   file, You can obtain one at http://mozilla.org/MPL/2.0/.
⍝
⍝ show all ⎕CR functions in GNU APL: ⎕CR ⍬
⍝ show structure: 8 ⎕CR X
⍝
TABLE_COLS←2
TABLE_ROWS←5
∇Z←HtmlTableRows X;CELLS
  CELLS←{'<td>',⍵,'</td>'}¨{(≢⍵)⍴⍵}¨X
  Z←{(⊂'<tr>'),((⊂'</tr>'),⍨⍵)},¨{'<td>',('</td>',⍨⍵)}¨,¨CELLS
∇
∇Z←HtmlTableRowsDyalog X;CELLS
  ⍝Z←∊'tr'∘Tag∘∊¨↓'td'∘Tag¨X
∇
∇Z←PlbDataRaw
  Z←TABLE_ROWS TABLE_COLS⍴"""
lang
para
APL
a
BQN
a
C
p
Erlang
f
  """
∇
  HtmlTableRows PlbDataRaw
)OFF

⍝╔════╦════╦════╦════╦════╦════╦════╦════╦════╦════╦════╦════╦════╦════════════╗
⍝║ ~  ║ !⌶ ║ @⍫ ║ #⍒ ║ $⍋ ║ %⌽ ║ ^⍉ ║ &⊖ ║ *⍟ ║ (⍱ ║ )⍲ ║ _! ║ +⌹ ║            ║
⍝║ `◊ ║ 1¨ ║ 2¯ ║ 3< ║ 4≤ ║ 5= ║ 6≥ ║ 7> ║ 8≠ ║ 9∨ ║ 0∧ ║ -× ║ =÷ ║ BACKSP     ║
⍝╠════╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═════════╣
⍝║       ║ Q  ║ W⍹ ║ E⍷ ║ R  ║ T⍨ ║ Y¥ ║ U€ ║ I⍸ ║ O⍥ ║ P⍣ ║ {⍞ ║ }⍬ ║         ║
⍝║  TAB  ║ q? ║ w⍵ ║ e∈ ║ r⍴ ║ t∼ ║ y↑ ║ u↓ ║ i⍳ ║ o○ ║ p⋆ ║ [← ║ ]→ ║ RETURN  ║
⍝╠═══════╩═╦══╩═╦══╩═╦══╩═╦══╩═╦══╩═╦══╩═╦══╩═╦══╩═╦══╩═╦══╩═╦══╩═╦══╩═╗       ║
⍝║ (CAPS   ║ A⍶ ║ S  ║ D  ║ F  ║ G  ║ H  ║ J⍤ ║ K⌸ ║ L⌷ ║ :≡ ║ "≢ ║ |⊣ ║       ║
⍝║  LOCK)  ║ a⍺ ║ s⌈ ║ d⌊ ║ f_ ║ g∇ ║ h∆ ║ j∘ ║ k' ║ l⎕ ║ ;⍎ ║ '⍕ ║ \⊢ ║       ║
⍝╠════════╦╩═══╦╩═══╦╩═══╦╩═══╦╩═══╦╩═══╦╩═══╦╩═══╦╩═══╦╩═══╦╩═══╦╩════╩═══════╣
⍝║        ║ < ¦║ Z  ║ Xχ ║ C¢ ║ V  ║ B£ ║ N  ║ M  ║ <⍪ ║ >⍙ ║ ?⍠ ║             ║
⍝║  SHIFT ║ > °║ z⊂ ║ x⊃ ║ c∩ ║ v∪ ║ b⊥ ║ n⊤ ║ m| ║ ,⍝ ║ .⍀ ║ /⌿ ║  SHIFT      ║
⍝╚════════╩════╩════╩════╩════╩════╩════╩════╩════╩════╩════╩════╩═════════════╝
