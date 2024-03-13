#
#   Copyright © 2024 Christopher Augustus
#
#   This Source Code Form is subject to the terms of the Mozilla Public
#   License, v. 2.0. If a copy of the MPL was not distributed with this
#   file, You can obtain one at http://mozilla.org/MPL/2.0/.
#
plb_impl_head←⟨
  "impl"    ,"prov"             ,"source" ,"lic"    ,"plat",  ,"run"
⟩
plb_impl_body←⟨13,≠plb_impl_head⟩⥊⟨
  "Aboa"    ,"aboa-erlang"      ,"Erlang" ,"wcl"    ,"lmw"    ,"os"
  "Aboa"    ,"aboa-racket"      ,"Racket" ,"wcl"    ,"lmw"    ,"os"
  "Aboa"    ,"aboa-s7"          ,"C"      ,"wcl"    ,"lmw"    ,"os"
  "APL"     ,"Dyalog"           ,"C"      ,"prop"   ,"lmw"    ,"ws"
  "APL"     ,"GNU APL"          ,"C"      ,"scl"    ,"lmw"    ,"ws"
  "BQN"     ,"BQN"              ,"JS"     ,"wcl"    ,"JS"     ,"ws"
  "BQN"     ,"CBQN"             ,"C"      ,"wcl"    ,"lmw"    ,"ws"
  "C"       ,"Clang"            ,"C"      ,"scl"    ,"lmw"    ,"os"
  "C"       ,"GCC"              ,"C"      ,"scl"    ,"lmw"    ,"os"
  "Erlang"  ,"OTP"              ,"C"      ,"wcr"    ,"lmw"    ,"os"
  "J"       ,"Jsource"          ,"C"      ,"wcr"    ,"lmw"    ,"ws"
  "Jelly"   ,"Jelly"            ,"Python" ,"wcr"    ,"lmw"    ,"vm"
  "Uiua"    ,"Uiua"             ,"Rust"   ,"wcr"    ,"lmw"    ,"ws"
⟩
plb_lang_head←⟨
  "lang"    ,"fam"    ,"para"   ,"type"   ,"mac"    ,"mem"    ,"plat"   ,plb_impl_head
⟩
plb_lang_data←plb_lang_head∾⟨7,≠plb_lang_head⟩⥊⟨
  "Aboa"    ,"Aboa"   ,"af"     ,"ds"     ,"gc"     ,"gc"     ,"os"     ,""
  "APL"     ,"APL"    ,"a"      ,"dw"     ,"gc"     ,"gc"     ,"os"     ,""
  "BQN"     ,"APL"    ,"a"      ,"ds"     ,"gc"     ,"gc"     ,"os"     ,""
  "C"       ,"C"      ,"p"      ,"sw"     ,"pp"     ,"alloc"  ,"os"     ,""
  "Erlang"  ,"Erlang" ,"f"      ,"ds"     ,"pp"     ,"gc"     ,"BEAM"   ,""
  "J"       ,"APL"    ,"a"      ,"ds"     ,"gc"     ,"gc"     ,"os"     ,""
  "Jelly"   ,"APL"    ,"a"      ,"ds"     ,"gc"     ,"gc"     ,"os"     ,""
  "Uiua"    ,"APL"    ,"a"      ,"ds"     ,"gc"     ,"gc"     ,"os"     ,""
⟩
