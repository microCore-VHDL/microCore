\ ----------------------------------------------------------------------
\ @file : disasm.fs
\ ----------------------------------------------------------------------
\
\ Last change: KS 16.02.2021 18:47:32
\ Project : microCore
\ Language : gforth_0.6.2
\ Last check in : $Rev: 645 $ $Date:: 2021-02-17 #$
\ @copyright (c): Free Software Foundation
\ @original author: ks - Klaus Schleisiek
\
\ @license: This file is part of microForth.
\ microForth is free software for microCore that loads on top of Gforth;
\ you can redistribute it and/or modify it under the terms of the
\ GNU General Public License as published by the Free Software Foundation,
\ either version 3 of the License, or (at your option) any later version.
\ This program is distributed in the hope that it will be useful,
\ but WITHOUT ANY WARRANTY; without even the implied warranty of
\ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
\ GNU General Public License for more details.
\ You should have received a copy of the GNU General Public License
\ along with this program. If not, see http://www.gnu.org/licenses/.
\
\ @brief : microCore disassembler with macro resolution.
\          DISASM displays one instruction per key pressed.
\          <cr> or <esc> to finish.
\
\ Version Author   Date       Changes
\   210     ks   14-Jun-2020  initial version
\ ----------------------------------------------------------------------
Only Forth also definitions 

Variable Expand  Expand off  \ OFF: display macros with macro name. 
                             \ ON: display every single instruction
Variable out     out off

: dis-cr     ( -- )            out off    _CR ;
: dis-emit   ( char -- )       1 out +!   _EMIT ; 
: dis-type   ( addr len -- )   dup out +! _TYPE ;
: position   ( n -- )
   out @ max BEGIN  dup out @ - WHILE  BL dis-emit  REPEAT  drop
;
: results    ( -- )   &18 position ;

: std-output ( -- )
   ['] _CR IS CR     ['] _EMIT IS EMIT     ['] _TYPE IS TYPE
;
: dis-output ( -- )
   ['] dis-cr IS CR  ['] dis-EMIT IS EMIT  ['] dis-TYPE IS TYPE
;
\ ----------------------------------------------------------------------
\ manipulating the stack field
\ ----------------------------------------------------------------------

: patch-stack  ( op stack -- op' )
   swap [ #stack not #codemask and ] Literal and or
;
: Stack: ( stack -- )  Create ,  Does> ( op -- op' )  @ patch-stack ;

#none Stack: NONE
#pop  Stack: POP
#push Stack: PUSH
#both Stack: BOTH

: .addr   ( addr -- )    0 <#  [ data_width 3 + 4 / ] Literal 0 DO  #  LOOP  #> type ." : " ;

: .opcode ( opcode -- )  0 <#  [ inst_width 3 + 4 / ] Literal 0 DO  #  LOOP  #> type space ;

: tdump  ( caddr quan -- )
   temp-hex   cells bounds
   ?DO  cr I .addr space
        I >memory $10 cells bounds
        DO  I @ .opcode  1 cells +LOOP
   $10 +LOOP
;
: .listname?  ( caddr link -- f )
   swap >r
   BEGIN  @ dup
   WHILE  dup cell- @ r@ =
          IF  cell- .wordname r>  EXIT THEN
   REPEAT  rdrop
;
: .listname  ( caddr link -- )   .listname? drop ;

: a-call?  ( caddr n -- caddr n ff | caddr+1 tf )
   over opcode@ op_CALL - IF  false  EXIT THEN
   op_CALL .opcode  results
   over + 1+   dup Colons .listname  tu. ." call"
   1+ true
;
: a-branch? ( opcode -- addr | ff )
   Branches
   BEGIN  @ dup
   WHILE  2dup 2 cells - @ #codemask and = IF  nip EXIT THEN
   REPEAT  nip
;
: a-condition?  ( caddr n -- caddr n ff | caddr+1 tf )
   over opcode@ a-branch? dup 0= ?EXIT
   >r  over opcode@ .opcode
   results  over + 1+
   dup Colons .listname   tu.
   r> 2 cells - .wordname
   1+ true
;
: literals? ( caddr -- caddr ff | caddr' tf )
   dup opcode@ #literal and 0= IF  false  EXIT THEN
   dup nibbles@  BEGIN  ?dup WHILE swap .opcode  1-  REPEAT
   dup >branch drop   swap nibbles@ nibbles>
   a-call?      IF  true  EXIT THEN
   a-condition? IF  true  EXIT THEN
   over opcode@ noop? IF  op_NOOP .opcode  swap 1+ swap  THEN
   results dup >r .   r@ 2 u>
   IF  Variables
      BEGIN  @ ?dup
      WHILE  dup cell- @ r@ = IF  3 cells - >name .name  true  rdrop EXIT THEN
      REPEAT
   THEN  Colons
   BEGIN  @ ?dup
   WHILE  dup cell- @ r@ = IF  ." ['] " 3 cells - >name .name  true  rdrop EXIT THEN
   REPEAT
   r> tu.   true
;
: .inst-name  ( opcode link -- opcode ff | tf )
   BEGIN  @ dup
   WHILE  2dup 2 cells - @ #opmask and =
          IF  results  2 cells - .wordname drop true  EXIT THEN
   REPEAT
;
: decode  ( opcode -- )
   Branches .inst-name ?EXIT
   BEGIN  Operators .inst-name ?EXIT
          dup #groupmask and [ #alu #push or ] Literal =
   WHILE  results ." 2dup " POP
   REPEAT
   .opcode
;
: macro?  ( caddr -- caddr' tf | caddr ff )
   Expand @ IF  False  EXIT THEN
   dup macro@ dup 0= ?EXIT  >r           \ macro# on rstack
   BEGIN  dup macro@ r@ =
   WHILE  dup opcode@ .opcode  1+
   REPEAT results  true
   Macro-link BEGIN  @ ?dup
   WHILE  dup cell- @ r@ =
          IF  2 cells - .wordname rdrop EXIT THEN
   REPEAT ." macro"  r> .
;
: .instruction  ( caddr -- caddr' )   temp-hex
   cr dup Colons .listname? IF  results ." ---------------" cr  THEN
   dup .addr
   macro?    ?EXIT
   literals? ?EXIT
   dup 1+ swap opcode@   dup .opcode  decode
;
: break-key?  ( -- f )   key dup #cr = swap #esc = or ;

: disasm  ( caddr -- )  \ displays one instruction per key pressed. <cr> or <esc> to finish
   dis-output
   BEGIN  .instruction  break-key? UNTIL
   drop space std-output
;
: show    ( <name> -- )   defined 0= ?missing   >body @ disasm ;
