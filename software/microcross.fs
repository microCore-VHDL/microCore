\ ----------------------------------------------------------------------
\ @file : microcross.fs
\ ----------------------------------------------------------------------
\
\ Last change: KS 10.04.2021 18:19:51
\ @project: microForth/microCore
\ @language: gforth_0.6.2
\ @copyright (c): Free Software Foundation
\ @original author: ks - Klaus Schleisiek
\ @contributor:
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
\ @brief : The microCore cross-compiler.
\
\ Version Author   Date       Changes
\   210     ks   14-Jun-2020  initial version
\   2200    ks   19-Oct-2020  Library mechanism integrated
\   2300    ks   18-Feb-2021  OOP mechanism integrated
\ ----------------------------------------------------------------------
Forth definitions

: .version  ( -- )  temp-decimal Version u. #BS emit [char] _ emit data_width . ;

\ Debugger forward references
Defer t_execute    :noname  true abort" t_execute not initialized" ;    IS t_execute
Defer >t           :noname  true abort" >t not initialized" ;           IS >t
Defer t>           :noname  true abort" t> not initialized" ;           IS t>
Defer >target      :noname  true abort" >target not initialized" ;      IS >target
Defer target>      :noname  true abort" target> not initialized" ;      IS target>
Defer send-image   :noname  true abort" send-image not initialized" ;   IS send-image
Defer find-methods :noname  true abort" find-methods not initialized" ; IS find-methods

Defer mark-target        ( -- )     ' noop IS mark-target         \ for loading libraries
Defer advance-libsource  ( xt -- )  ' drop IS advance-libsource   \ for loading libraries

Variable (doGoto   (doGoto   off  : doGoto   ( -- xt )  (doGoto @ ;
Variable (doLabel  (doLabel  off  : doLabel  ( -- xt )  (doLabel @ ;
Variable (doColon  (doColon  off  : doColon  ( -- xt )  (doColon @ ;
Variable (doLibdef (doLibdef off  : doLibdef ( -- xt )  (doLibdef @ ;
Variable (doCreate (doCreate off  : doCreate ( -- xt )  (doCreate @ ;

\ ----------------------------------------------------------------------
\ instruction fields as defined by the hardware architecture
\ ----------------------------------------------------------------------

 $80 Constant #literal
 $60 Constant #type
 $18 Constant #stack
 $07 Constant #group
 $F8 Constant #groupmask
 $1F Constant #usrmask
\ 
\ type field
 $00 Constant #bra
 $20 Constant #alu
 $40 Constant #mem
 $60 Constant #usr
\ 
\ stack field
 $00 Constant #none
 $08 Constant #pop
 $10 Constant #push
 $18 Constant #both

\ check digits
$101 Constant #if
$202 Constant #else
$303 Constant #begin
$404 Constant #while
$505 Constant #for
$606 Constant #colon
$707 Constant #trap
$808 Constant #?for
$909 Constant #host
$A0A Constant #macro
$B0B Constant #lib       \ when loading from library predefinitions

prog_addr_width 2**                     Constant #maxprog
data_addr_width 2**                     Constant #maxdata    \ size of data memory
data_width 8 /mod swap 0= 1+ +          Constant #bytes/cell
data_width 2** 1-                       Constant #datamask
data_width 1- 2**                       Constant #signbit
inst_width 1- 2** 1-                    Constant #opmask
inst_width 2** 1-                       Constant #codemask
inst_width 1-                           Constant #lit-width
inst_width 1 - 2** 1-                   Constant #litmask
inst_width 2 - 2**                      Constant #litmod
data_width #lit-width /mod swap 0= 1+ + Constant #nibbles    \ nibbles needed to represent any number

\ ----------------------------------------------------------------------
\ accessing the target's program memory
\ ----------------------------------------------------------------------

Create Memory   #datamask #maxprog umin 5 + cells allot

Variable Verbose   Verbose off
Variable Tcp                              \ TargetCodePointer
Variable Transferred                      \ Target image transfer pointer
Variable Tdp                              \ TargetDataPointer
Variable Sequential                       \ number of past opcodes available for peep-hole optimization
Variable Targeting   Targeting off        \ differentiates between cross-compilation and Macro/Compiler extensions

Variable Libfile     Libfile off          \ pointer to library filepath string ( libpath )
Variable Libload     2 cells allot        \ | flag | host xt | target xt |
Variable Trash                            \ Dummy countfield when not lib-loading
Variable Countfield  1 cells allot        \ Holds countfield and countfield address during lib-loading
Create Tmarker  ( here ) 0 , ( there ) 0 , ( depth ) 0 , ( Current ) 0 , ( save-input stream ) 6 cells allot

Variable Class-context  Class-context off \ Momentary class context
Variable Macro      Macro off             \ holds the current macro tag to be included in code compilation. 0 => no macro
Variable Macro#     Macro# off            \ holds the last assigned macro number
$FF000000         Constant #macromask     \ macro# is embedded in opcode field
#macromask not    Constant #linkmask      \ unresolved link in lower 24 bits of the target memory

Variable Macro-link Macro-link off        \ linked list of all macros
Variable Labels     Labels off            \ linked list of all labels
Variable Colons     Colons off            \ Builds a linked list of :-definitions for the disassembler
Variable Operators  Operators off         \ Linked list of all operators, used by the disassembler
Variable Branches   Branches off          \ Linked list of all branch operators, used by the disassembler
Variable Constants  Constants off         \ linked list of all constants
Variable Variables  Variables off         \ linked list of all creates and variables
Variable Doers      Doers off             \ linked list of all Create ... Does> objects
Variable Init-link  Init-link off         \ linked list of all init: definitions
Variable Goto-nibbles                     \ default number of nibbles to be used for forward referencing GOTO and ?GOTO
   prog_addr_width 1+ #lit-width /mod swap 0= 1+ + Goto-nibbles !

: >memory  ( caddr -- addr )   cells Memory + ;
: t!       ( n caddr -- )      >memory ! ;
: t@       ( caddr -- n )      >memory @ ;
: opcode@  ( caddr -- op )     t@ #codemask and ;
: macro@   ( caddr -- # )      t@ -&24 shift ;
: macro!   ( n caddr -- )      swap Macro @ &24 shift or swap t! ;
: link@    ( caddr -- link )   t@ #linkmask and ;
: there    ( -- addr )         Tcp @ ;
: tallot   ( n -- )            dup Sequential @ + 0 max Sequential !  Tcp +!
   there #maxprog u< ?EXIT cr ." there: " there u. ." program memory full" abort
;
: t,       ( n -- )
   dup [ #codemask not ] Literal and abort" not an opcode"
   there macro!  1 tallot
;
: trap-addr ( n -- addr )     trap_width 2** * ;

\ ----------------------------------------------------------------------
\ peep-hole optimizer
\ ----------------------------------------------------------------------

: |        ( -- )              Sequential off ;   \ break peep-hole optimizer

: prev@    ( -- opcode | 0 )   Sequential @     IF  there 1 - t@ #codemask and EXIT THEN  false ;

: pprev@   ( -- opcode | 0 )   Sequential @ 1 > IF  there 2 - t@ #codemask and EXIT THEN  false ;

: ppprev@  ( -- opcode | 0 )   Sequential @ 2 > IF  there 3 - t@ #codemask and EXIT THEN  false ;

: Match  ( opcode -- )    Create  #codemask and ,
Does>    ( opcode -- f )  @ xor #codemask and 0= ;

op_NOOP   Match noop?
op_DUP    Match dup?
op_QDUP   Match ?dup?
op_ZEQU   Match 0=?
op_ZLESS  Match 0<?
op_CALL   Match call?
op_OVER   Match over?
op_SWAP   Match swap?
op_RPUSH  Match >r?
op_RPOP   Match r>?
op_ADD    Match plus?
op_SUB    Match minus?
op_EXIT   Match exit?

\ ----------------------------------------------------------------------
\ Target literals and addresses are composed of literal-nibbles
\ preceeding opcodes which use them
\ ----------------------------------------------------------------------

: /nibble  ( lit -- nib lit' )
   dup #litmask and #literal or   swap #literal /
;
: last-nibble?  ( nib lit -- f )
   0 case? IF  #litmod and 0=  EXIT THEN
   -1    = IF  #litmod and     EXIT THEN
   drop False
;
: >nibbles  ( lit -- nib_q .. nib_1 q )  \ convert literal into q nibbles
   0 >r  BEGIN  r> 1+ >r   /nibble  2dup last-nibble? UNTIL  drop r>
;
: nibbles  ( lit -- quan )  \ number of nibbles needed to represent lit
   0 >r  BEGIN  r> 1+ >r   /nibble  tuck last-nibble? UNTIL  drop r>
;
: *nibble  ( nib lit' -- lit )
   #literal *   swap #litmask and or
;
: nibbles>  ( nib_q .. nib_1 q -- lit )  \ convert q nibbles into literal
   dup 0= ABORT" no nibbles available"
   >r  #litmask and
   dup #litmod and IF  #literal negate or  THEN
   r> 1 ?DO  *nibble  LOOP
;
: nibbles!  ( nib_q .. nib_1 quan caddr -- )  swap bounds DO  I macro!  LOOP ;

: lit-op?  ( op -- f )  #literal and 0<> ;

: ?noop,  ( -- )      prev@ #literal and 0= ?EXIT  op_NOOP t, ;

: nibbles,  ( nib_q .. nib_1 quan -- )  ?noop, there over tallot nibbles! ;

: nibble-count  ( caddr -- quan )
   dup  BEGIN  dup t@ lit-op? WHILE  1+  REPEAT  swap -
;
: nibbles@  ( caddr -- nib_q .. nib_1 quan )
   dup  BEGIN  dup t@ lit-op? WHILE  dup t@ >r  1+  REPEAT
   swap -   dup  BEGIN  ?dup WHILE  1-  r> -rot  REPEAT
;
: lit-addr ( caddr -- caddr' | 0 )  \ search for the beginning of a sequence of lits
   >r 0
   BEGIN  r@ over - t@ lit-op? WHILE  1+  REPEAT
   dup IF  r@ swap 1- -  THEN  rdrop
;
: last-lit  ( -- lit )
   there 1- lit-addr ?dup 0= ?EXIT nibbles@ dup negate tallot nibbles>
;
: nibbles-needed ( caddr -- quan )  nibbles@ nibbles> nibbles ;

: ?literal ( lit -- lit' )   \ range check and sign extension
   dup    dup 0< IF  #datamask or 1+
          ELSE  [ #datamask not ] Literal and
          THEN  IF  cr ." number beyond data path width " u. ABORT THEN
   dup #signbit and
   IF  [ #datamask not ] Literal or  ELSE  #datamask and  THEN   \ sign extension
;
: lit, ( n -- )   \ compiles a literal into the minimum number of nibbles.
   ?literal >nibbles nibbles,
;
: fixed,  ( n quan -- )      \ compiles a literal into quan nibbles.
   >r ?literal r@ 0 ?DO  /nibble  LOOP
   2dup last-nibble? 0= abort" literal does not fit into fixed field"
   drop  r> nibbles,
;
\ ----------------------------------------------------------------------
\ Opcode-field compilers and Opcode compiler
\ ----------------------------------------------------------------------

: tempcode  ( addr -- )
   there >r   @ dup t,
   Macro @ IF  rdrop drop EXIT THEN  op_EXIT t,
   there r@ -   r> Tcp !
   there swap false send-image
\   Verbose @ IF  cr dup . ." instruction "  THEN
   drop   there t_execute
;
: Op:  ( n <name> (comment <name> -- )
   Create ,                 \ here the instruction code is layed down which will be compiled into the target memory
   postpone (               \ skip over comment )
   also Forth ' , previous  \ here the xt of the word is layed down which will be executed during interpretation in the target
   here  Operators @ , Operators !
Does> ( -- )
   comp? IF  @ t,  EXIT THEN
   dbg? IF  tempcode  EXIT THEN
   cell+ @ execute
;
: don't   ( -- )  True abort" can not be executed" ;

: Brn:    ( n <name> (comment <name> -- )
   Create ,                 \ here the instruction code is layed down which will be compiled into the target memory
   postpone (               \ skip over comment )
   also Forth ' , previous  \ here the xt of the word is layed down which will be executed during interpretation in the target
   here  Branches @ , Branches !
Does> ( -- )
   comp? IF  @ t,  EXIT THEN
   dbg? IF  tempcode  EXIT THEN
   cell+ @ execute
;
: (Macro: ( <name> -- addr ) \   | xt | macro# | link |
   Create   here 0 ,                          \ Placeholder for macro's xt
   1 Macro# +!   Macro# @ ,   Forth
   here Macro-link @ , Macro-link !
Does> ( -- )   Macro @ IF  @ execute  EXIT THEN      \ another macro inside the macro being defined
   dup cell+ @ Macro !                 ( pfa )       \ set Macro to actual Macro#
   dbg? IF  ]   there                  ( pfa there ) \ start compiling the macro code as a dummy definition, leave start address
            swap @ execute   Macro off ( there )     \ produce the code and reset Macro
            op_EXIT t,   postpone [    ( there )     \ append an EXIT finishing compilation
\            Verbose @ IF  ." macro# " Macro @ .  THEN
            there over -   swap Tcp !  ( length )    \ compute code's length and reset Tcp
            there swap false send-image              \ transfer code into the target's program memory
            there t_execute
        EXIT THEN                      ( pfa )
   @ execute   Macro off                             \ produce macro code and reset Macro
;
gforth_062 [IF]
: .macros    ( -- )  Macro-link BEGIN  @ ?dup WHILE  dup 4 cells - >name .name  REPEAT ;
: .variables ( -- )  Variables  BEGIN  @ ?dup WHILE  dup 3 cells - >name .name  REPEAT ;
: .constants ( -- )  Constants  BEGIN  @ ?dup WHILE  dup 3 cells - >name .name  REPEAT ;
: .colons            Colons     BEGIN  @ ?dup WHILE  dup 3 cells - >name .name  REPEAT ;
: .inits     ( -- )  Init-link  BEGIN  @ ?dup WHILE  dup 4 cells - >name .name  REPEAT ;
[THEN] gforth_079 gforth_072 or [IF]
: .macros    ( -- )  Macro-link BEGIN  @ ?dup WHILE  dup 3 cells - >name .name  REPEAT ;
: .variables ( -- )  Variables  BEGIN  @ ?dup WHILE  dup 2 cells - >name .name  REPEAT ;
: .constants ( -- )  Constants  BEGIN  @ ?dup WHILE  dup 2 cells - >name .name  REPEAT ;
: .colons            Colons     BEGIN  @ ?dup WHILE  dup 2 cells - >name .name  REPEAT ;
: .inits     ( -- )  Init-link  BEGIN  @ ?dup WHILE  dup 3 cells - >name .name  REPEAT ;
[THEN]
\ ----------------------------------------------------------------------
\ Arithmetic optimizer, looking for "2dup <arithop>" and "swap -"
\ ----------------------------------------------------------------------

: 2dup?    ( -- f )   prev@ over? EXTENDED and IF  pprev@ over?  EXIT THEN  false ;

: Arith:  ( n <name> (comment <name> -- )  Op:
Does> ( -- )
   comp? IF  @   2dup? IF  -2 tallot | #both xor  THEN  t,  EXIT THEN
   dbg? IF  tempcode  EXIT THEN
   cell+ @ execute
;
: Sub:  ( n <name> (comment <name> -- )  Op:
Does> ( -- )
   comp? IF  @   prev@ swap? IF  [ #group not ] Literal and op_SSUB #group and or  -1 tallot  THEN
             2dup? IF  -2 tallot | #both xor  THEN  t,
         EXIT THEN
   dbg? IF  tempcode  EXIT THEN
   cell+ @ execute
;
\ ----------------------------------------------------------------------
\ stack optimizers
\ ----------------------------------------------------------------------

: Drop: ( n <name> (comment <name> -- ) Op:
Does> ( -- )
   comp? IF  @  EXTENDED IF  prev@ swap? IF  -1 tallot  drop op_NIP   THEN THEN
             t,  EXIT
         THEN
   dbg? IF  tempcode  EXIT THEN
   cell+ @ execute
;
: Over: ( n <name> (comment <name> -- ) Op:
Does> ( -- )
   comp? IF  @  EXTENDED IF  prev@ swap? IF  -1 tallot  drop op_TUCK  THEN THEN
             t,  EXIT
         THEN
   dbg? IF  tempcode  EXIT THEN
   cell+ @ execute
;
: Swap: ( n <name> (comment <name> -- ) Op:
Does> ( -- )
   comp? IF  @  prev@ swap? IF  -1 tallot drop EXIT THEN              \ throw out "swap swap"
             EXTENDED
             IF  prev@ over? IF  -1 tallot  drop op_UNDER  THEN THEN  \ "over swap" = "under"
             t,  EXIT
         THEN
   dbg? IF  tempcode  EXIT THEN
   cell+ @ execute
;
\ ----------------------------------------------------------------------
\ "r> >r" is a noop
\ ----------------------------------------------------------------------

: Tor:  ( n <name> (comment <name> -- ) Op:
Does> ( -- )
   comp? IF  @  prev@ r>? IF  -1 tallot drop  EXIT THEN   t,  EXIT THEN
   dbg? IF  tempcode  EXIT THEN
   cell+ @ execute
;
\ ----------------------------------------------------------------------
\ replace "call exit" by branch
\ ----------------------------------------------------------------------

: Ex: ( n <name> (comment <name> -- ) Op:
Does> ( -- )
   comp? IF  @  prev@ call? IF  -1 tallot drop op_BRANCH  THEN   t,  EXIT THEN
   dbg? IF  tempcode  EXIT THEN
   cell+ @ execute
;
\ ----------------------------------------------------------------------
\ Create in the Target
\ ----------------------------------------------------------------------

: Tcreate  ( -- )   ?exec
   Trash Countfield !   >in @ >r   have ?dup
   IF  dup cell+ @ doLibdef =
      IF  Verbose @ IF  cr ." LibCreate "  dup >name .name  THEN
          r> >in !   dup advance-libsource   name 2drop
          cell+ doCreate over !   cell+ Tdp @ over !
          cell+ Variables @ over !   Variables !
          Dp @ Tmarker !                          \ Advance Tmarker
      EXIT THEN
      drop
   THEN   r> >in !
   Tdp @ Constant   here Variables @ , Variables !
Does> @ ( -- addr )  [ here cell- (doCreate ! ]
   comp? IF  lit,  EXIT THEN    dbg? IF  >t  THEN
;
\ ----------------------------------------------------------------------
\ Optimizer: Zwei Literale hintereinander gefolgt von "+" während der
\ Kompilation addieren und zu einem Literal machen
\ ----------------------------------------------------------------------

\ : prevlit? ( -- caddr | 0 )
\   prev@ noop? IF  2
\      BEGIN  Sequential @ over >   there over - t@ literal? and WHILE  1+  REPEAT
\      1- dup 1 > IF  there swap -  EXIT THEN  drop
\   THEN   false
\ ;
\ : Add:  ( n <name> -- )  Op:
\ Does> ( -- )
\   comp? IF  @   2dup? IF  -2 tallot | #both xor  THEN  t,  EXIT THEN
\   dbg? IF  tempcode  EXIT THEN
\   cell+ @ execute
\ ;
\
\ : .lit    ( caddr -- )   nibbles@ nibbles> . ;

Vocabulary Target          \ Vocabulary for all Target definitions

: Method ( -- )   Class-context off ;

: Macro:  ( <name> -- context addr :noname # )
   get-order   Current @   [ ' parser >body ] Literal @
   'interpreter @   'compiler @   Targeting @
   (Macro: :noname #macro   postpone Method
;
: Host:  ( <name> -- # )  : drop #host   postpone Method ;

: ;  ( # -- )  \ redefine ; so that it can be used to terminate macro definitions
   #macro case? IF  postpone ;  ( context addr xt -- )    swap !
                    Targeting !   'compiler !   'interpreter !
                    IS parser   Current !   set-order
                EXIT THEN
   #host  case? IF  Target  0  THEN  postpone ;
; immediate

Root definitions

' Forth  Alias H immediate \ "Host" - short-hand definition to switch context also while compiling
' Target Alias T immediate
' Root   Alias Root
: ..  ( -- )   Method ; immediate

\ ----------------------------------------------------------------------
\ Opcodes are defined in opcodes.fs
\ ----------------------------------------------------------------------
Target definitions

include opcodes.fs

\ ----------------------------------------------------------------------
\ branching
\ Due to relative addressing, the number of nibbles needed for a branch address
\ varies. At first, only one nibble is assumed to be sufficient to hold the offset.
\ When the compiler finds that more nibbles are needed, the source code is re-interpreted
\ ----------------------------------------------------------------------
Forth definitions

Variable Prefix     Prefix off     \ Prefix codes for conditionals to handle e.g. "carry IF" etc.

 0 Constant #if-bit
 1 Constant #nif-bit
 2 Constant #z-bit
 3 Constant #nz-bit
10 Constant #docall
11 Constant #branch
12 Constant #addr

: complement      ( -- )    Prefix @ #docall < IF  Prefix @ 1 xor Prefix !  THEN ;

: if-prefix       ( -- )    #if-bit Prefix ! ;

: ?strange-prefix ( n -- )  ?dup 0= ?EXIT  if-prefix  cr ." Prefix " dup . abort" unexpected Prefix " ;

: >branch  ( caddr -- br.addr quan.nibbles )  \ length of branch lit field
   0  BEGIN  over t@ lit-op? WHILE  1+ swap  1+ swap  REPEAT
;
: <mark    ( -- sca caddr )    ?noop, |  source> there ;

: <resolve ( sca caddr prefix -- )  \ ProgramCounter points behind branch opcode
   ?noop, |   1 >r                   \ minimum length of branch
   BEGIN  over there r@ + 1+ -      \ branch offset
          nibbles r@ >              \ does branch offset fit?
   WHILE  r> 1+ >r                  \ try with offset one nibble longer
   REPEAT
   drop  ( get rid of prefix )   
   there r@ + 1+ -  r> fixed,  drop-source
;
: >mark    ( quan.nibbles prefix -- sca caddr )
   ?noop, |   swap >r   source> there
   rot drop  ( get rid of prefix )
   #literal r> 0 ?DO  dup t,  LOOP  drop
;
: >resolve ( sca caddr -- quan.nibbles.needed | 0 )
   ?noop, |  dup >r  dup >branch >r  \ R: offset width allocated
   swap t@ #litmask and +           \ fetch numerical value of first emplaced branch offset nibble and correct branch start address
   there swap 1+ - >nibbles         \ branch offset
   dup r> -                         \ does offset fit?
   IF  dup >r ndrop                 \ remove nibbles
       r> r> Tcp !  swap >source  EXIT
   THEN
   r> nibbles!  drop-source  false
;
: conditional,  ( -- )  Prefix @  if-prefix
   #docall  case? IF  T JSR       H  EXIT THEN
   #branch  case? IF  T branch    H  EXIT THEN
   #if-bit  case? IF  T 0=branch  H  EXIT THEN   \ consumes flag
   #addr    case? IF  T noop      H  EXIT THEN
   ?strange-prefix
;
: (if   ( quan.offset -- sca caddr # )   Prefix @ >mark conditional, #if ;

: (else ( quan.offset -- sca caddr # )   #branch >mark T branch H #else ;

\ ----------------------------------------------------------------------
\ Due to the variable length branch offset fields ELSE does not compile anything
\ This is handled by THEN instead. ELSE leaves its unique check digit. Therefore,
\ THEN "knows" when it was preceeded by an ELSE.
\
\ THEN tests the check digit: If it was #ELSE,
\ the branch between ELSE ... THEN is compiled first yielding the actual branch
\ offset needed for ELSE. Thereafter, the IF ... ELSE branch is compiled.
\ ----------------------------------------------------------------------

: optimize-?goto  ( -- )
   Prefix @  #if-bit - ?EXIT
   prev@ 0=? IF  -1 tallot  ELSE  T 0= H  THEN
   #nif-bit Prefix !
;
: unresolved-goto   ( link -- caddr )
   there swap there macro!  1 tallot
   Goto-nibbles @ 1 ?DO #literal t, LOOP
   Prefix @   if-prefix
   #addr    case? IF  T noop      H  EXIT THEN
   #docall  case? IF  T jsr       H  EXIT THEN
   #branch  case? IF  T branch    H  EXIT THEN
   #nif-bit case? IF  T 0=branch  H  EXIT THEN
   ?strange-prefix
;
: resolved-goto ( caddr -- )
   Prefix @ #addr = IF  lit,  EXIT THEN
   complement 0 swap Prefix @ <resolve conditional,
;
: resolve-goto  ( caddr -- )
  >r  there r@    r@ 1+ nibble-count 1+ >r                \ determine # of lit-nibbles allocated
  r@ +   dup t@ noop? IF  2drop there  ELSE  1+ -  THEN   \ abs. address when NOP, relative branch otherwise
  dup nibbles r@ > abort" goto offset out of range, increase Variable Goto-nibbles"
  r@ 0 ?DO  /nibble  LOOP drop   r> r> nibbles!
;
Target definitions Forth

: IF    ( -- sca caddr #if )  ?comp 1 (if ;

: ELSE  ( sca1 caddr1 #if -- sca1 caddr1 sca2 caddr2 #else )
   ?comp #if ?pairs  1 (else
;
: THEN ( sca caddr #if | sca1 caddr1 sca2 caddr2 #else -- )
   ?comp #if case? IF  >resolve ?dup 0= ?EXIT  (if  EXIT THEN
         #else ?pairs
   dup >r  >resolve ?dup IF  rdrop (else  EXIT THEN
   r>   there >r   >branch drop 1+ Tcp !
   >resolve ?dup IF  rdrop (if  EXIT THEN   r> Tcp !
;
: BEGIN ( -- sca caddr #begin )   ?comp <mark #begin ;

: WHILE ( sca1 caddr1 #begin -- sca2 caddr2 sca1 caddr1 #while )
   ?comp #begin ?pairs  T IF H drop 2swap #while
;
: REPEAT ( sca caddr #begin | sca2 caddr2 sca1 caddr1 #while -- )
   ?comp -rot  #branch <resolve T branch H
   #begin case? ?EXIT
   #while ?pairs >resolve ?dup IF  (if  THEN
;
: UNTIL  ( sca caddr #begin -- )
   ?comp  #begin ?pairs   Prefix @ <resolve conditional,
;
: FOR    ( -- sca caddr #for )
   ?comp prev@ r>? IF  -1 tallot  ELSE  T >r H  THEN  <mark #for ;

: ?FOR   ( -- sca2 caddr2 sca1 addr1 #?for )
   ?comp prev@ r>? IF  -1 tallot  ELSE  T >r H  THEN  1 (else drop <mark 2swap #?for ;

: NEXT   ( sca caddr #for | sca2 caddr2 sca1 caddr1 #?for -- )
   ?comp #for case? IF  #nz-bit <resolve T tor-branch H EXIT THEN
        #?for ?pairs    #if T THEN H #nz-bit <resolve T tor-branch H
;
EXTENDED [IF]
   : ?EXIT  ( f -- )
      ?comp Prefix @   if-prefix
      #if-bit  case? IF  T nz-exit H EXIT THEN
      ?strange-prefix
   ;
[ELSE]
   Macro: ?EXIT  ( f -- )  ?comp 1 lit, T 0=BRANCH EXIT H ;
[THEN]

\ ----------------------------------------------------------------------
\ Data memory and system initialization.
\ The phrase CALL initialization in the boot routine will initialize data memory.
\
\ Anchored at Initials, initialized allocates the following data structure:
\ | link to next datastructure | starting DP | ending DP |
\ uninitialized fills in the ending DP
\ a following initialized allocates another data structure
\ ----------------------------------------------------------------------
Forth definitions

Create Data     #maxdata cells allot    \ shadow data memory

: >data     ( daddr -- addr )  dup #maxdata u> abort" data memory access out of range"  cells Data + ;
: d!        ( n daddr -- )     >data ! ;
: d@        ( daddr -- n )     >data @ ;

Create Initials  0 , 0 , 0 ,  \ linked list for data memory initialization

: last-initial   ( -- addr )  Initials BEGIN  dup @ WHILE  @  REPEAT ;
: initialized?   ( -- f )     last-initial cell+ @ ;
: uninitialized? ( -- f )     last-initial cell+ cell+ @ ;

: save-dp-block  ( from to -- )
   over lit,
   swap ?DO  I d@ lit, T op_DATA t, H  LOOP       \ save values in progmem
   T op_DROP t, H
;
: compile-inits  ( -- )
   0 Init-link BEGIN  @ ?dup WHILE  dup  REPEAT
   ]  BEGIN  ?dup WHILE  4 cells - execute  REPEAT   postpone [
;
: save-data   ( -- )
   Initials BEGIN  dup cell+ @ ?dup               \ anything to initialize?
      IF not   over cell+ cell+ @ ?dup 0=
         IF  Tdp @ not  THEN  not  save-dp-block
      THEN
      @ ?dup 0=
   UNTIL  compile-inits  T op_EXIT t, H
;
\ ----------------------------------------------------------------------
\ data and program memory origins
\ ----------------------------------------------------------------------

Variable (signed   (signed on   \ interpret numbers as signed or unsigned

: signed   ( -- )   (signed on ;
: unsigned ( -- )   (signed off ;
: signed?  ( -- f ) (signed @ ;

: signextend ( t_n -- h_n )  dup #signbit and ?dup IF  negate or  THEN ;

: ?signed  ( t_n -- h_n )  signed? IF  signextend  THEN ;

: host-target  ( -- u )   cell_width data_width - dup 0< abort" target data width > host data width" ;

: dtarget ( high low -- dtarget )    \ handles different word widths on host and target
   swap signextend swap
   host-target 0 ?DO  2*  LOOP swap
   host-target 0 ?DO  d2/  LOOP
;
: udtarget ( high low -- udtarget ) \ handles different word widths on host and target
   host-target 0 ?DO  2*  LOOP swap
   host-target 0 ?DO  ud2/  LOOP
;
: tu.  ( addr -- )  #datamask and u. ;

\ ----------------------------------------------------------------------
\ data and program memory origins and data memory initialization
\ ----------------------------------------------------------------------
Target definitions Forth

: code-origin ( caddr -- )
   ?exec dbg? IF t> THEN
   Tcp @ abort" CODE-ORIGIN can only be used once."
   Tcp !
;
: data-origin ( taddr -- )  ?exec dbg? IF t> THEN  Tdp ! ;

: code@       ( -- caddr )  ?exec Tcp @ dbg? IF  >t  THEN ;

: trap-addr   ( n -- addr ) ?exec trap-addr dbg? IF  >t  THEN ;

: initialized  ( -- )
   initialized? 0= IF  Tdp @ not last-initial cell+ !  EXIT THEN
   uninitialized? ?dup 0= ?EXIT  not
   Tdp @ = IF  0 last-initial cell+ cell+ !  EXIT THEN  \ continue previous block
   here 0 , Tdp @ not , 0 , last-initial !
;
: uninitialized ( -- )  initialized? 0= ?EXIT
   Tdp @ not last-initial cell+ cell+ !
;
\ ----------------------------------------------------------------------
\ Target defining words
\ ----------------------------------------------------------------------

: Constant    ( n -- )
   dbg? IF t> THEN  Constant
   here  Constants @ , Constants !
Does> @        ( -- n )
   comp? IF  lit,  EXIT THEN    dbg? IF  >t  THEN
;
: 2constant    ( n1 n2 -- )
   dbg? IF t> t> swap THEN   Constant ,
   here  Constants @ , Constants !
Does> dup @ swap cell+ @   ( -- n2 n1 )
   comp? IF  lit, lit,  EXIT THEN
    dbg? IF  >t >t      EXIT THEN
   swap
;
: Version     ( f -- ) Constant immediate ;

: Variable    ( -- )   Tcreate   1 Tdp +! ;

: Register    ( addr -- )
   dbg? IF t> THEN  Constant
   here  Variables @ , Variables !
Does> @       ( -- n )
   comp? IF  lit,  EXIT THEN   dbg? IF  >t  THEN
;
: Bit         ( n -- )
   dbg? IF t> THEN  2** Constant
   here  Constants @ , Constants !
Does> @        ( -- n )
   comp? IF  lit,  EXIT THEN   dbg? IF  >t  THEN
;
\ ----------------------------------------------------------------------
\ Label ... GOTO
\ ----------------------------------------------------------------------

: Label       ( <name> -- )
   |  source>   have   swap >source
   IF  '   dup cell+ @ doGoto - abort" not a goto"
       doLabel over cell+ !                                  \ converts "goto" into a "label"
       >body dup @ there rot !                               \ patch the real target address
       BEGIN  dup link@ swap resolve-goto ?dup 0= UNTIL      \ resolve all forward references
   EXIT THEN
   Latest   there Constant   Last !
   here Labels @ , Labels !
Does> ( -- caddr ) [ here (doLabel ! ]   @ 
   comp? IF  lit,  EXIT THEN   dbg? IF  >t  THEN
; immediate

: ?GOTO       ( <name> -- )
   optimize-?goto   ?noop, |
   source> have swap >source
   IF  '   dup cell+ @
       doGoto case? IF ( goto-xt )  >body  dup @ unresolved-goto  swap !  EXIT
                    THEN
       dup doLabel = swap doColon = or 0= abort" not a label or colon definition"
       >body @ resolved-goto
       EXIT
   THEN
   Latest   0 unresolved-goto Constant   Last !
   here Labels @ , Labels !
Does> ( -- ) [ here (doGoto ! ]
   abort" unresolved forward reference"
; immediate

: GOTO        ( <name> -- )     #branch Prefix ! T postpone ?goto H ; immediate

: ADDR        ( <name> -- )       #addr Prefix ! T postpone ?goto H ; immediate

: CALL        ( <name> -- )     #docall Prefix ! T postpone ?goto H ; immediate

\ ----------------------------------------------------------------------
\ Immediate Target words
\ ----------------------------------------------------------------------

: recursive  ( -- )   ?comp reveal ; immediate

: recurse   ( -- )
   ?comp source>   Latest name>int >body @   #docall <resolve T JSR H
; immediate

: Literal ( n -- )  ?comp
   Targeting @ 0= IF  postpone Literal postpone lit,  EXIT THEN
   Debugging @ IF  t>  THEN  lit,
; immediate

: '     ( <name> -- xt )  ?exec ' >body @ ;

: [']   ( <name> -- xt )  ?comp ' >body @ lit, ; immediate

: [char] ( <c> -- )       ?comp bl parse drop c@ lit,
; immediate

: char   ( <c> -- n )     ?exec bl parse drop c@ dbg? IF  >t  THEN ;

\ ----------------------------------------------------------------------
\ Words which are useful to execute interactively during target compilation.
\ To be extended when the need arises
\ ----------------------------------------------------------------------

' Alias       Alias Alias
' 2**         Alias 2**
' 2//         Alias 2//
' immediate   Alias immediate
' [           Alias [       immediate
' ]           Alias ]
' |           Alias |       immediate
' include     Alias include
' .macros     Alias .macros
' .variables  Alias .variables
' .constants  Alias .constants
' .colons     Alias .colons
' .inits      Alias .inits
' binary      Alias binary
' decimal     Alias decimal
' hex         Alias hex
' signed      Alias signed
' unsigned    Alias unsigned
' .version    Alias .version
' .(          Alias .(
' (*          Alias (*
' [IF]        Alias [IF]    immediate
' [NOTIF]     Alias [NOTIF] immediate
' [ELSE]      Alias [ELSE]  immediate
' [THEN]      Alias [THEN]  immediate
' (           Alias (       immediate
' \           Alias \       immediate
' \\          Alias \\

\ ----------------------------------------------------------------------
\ for interactive use and inside Create ... Does> in the target
\ ----------------------------------------------------------------------

: allot  ( n -- )            Tdp +! ;

: ,      ( n -- )            Tdp @   1 Tdp +!   d! ;

: !      ( n addr -- )       dbg? comp? or IF  T ! H  EXIT THEN  d! ;

: @      ( addr -- n )       dbg? comp? or IF  T @ H  EXIT THEN  d@ ;

\ ----------------------------------------------------------------------
\ Trap and Method defining words
\ ----------------------------------------------------------------------

: TRAP:  ( <name> trap-number -- there trapaddr #trap )
   dup #usrmask > abort" USR number out of range"
   ?exec Create dup #usr or ,
   trap-addr  there  over Tcp !
   swap ] #trap |                       \ leaves there, addr and #trap for check by ;TRAP
   ['] don't ,                          \ embed "don't" to be consistent with OP:-definitions
   here  Operators @ , Operators !
Does> ( -- )
   comp? IF  @ t,  EXIT THEN
   dbg? IF tempcode EXIT THEN
   don't
;
: ;    ( i*x # -- )
   ?comp   #colon case?
   IF  prev@ call? IF  -1 tallot T branch H
                   ELSE  prev@ >r? IF  -1 tallot ?noop, T branch H
                                   ELSE  T exit H
                   THEN            THEN
       postpone [ reveal
       dbg? IF  Transferred @ there over - false send-image
                there Transferred !
       THEN
   EXIT THEN
\ Tmarker: | here | there | depth | Current | 6 cells save-input stream |
   #lib  case? IF  Libload cell+ >r
                   r@ @ cell+ doColon over !           \ rewrite do xt
                   cell+   r> cell+ @ over !           \ fill in target xt
                   cell+   Colons @ over !   Colons !  \ link into colons list
                   T exit H   postpone [
                   Countfield 2@ !                     \ make name visible again
                   Tmarker   Dp @ over !               \ Advance Tmarker
                   cell+   Tcp @ swap !
               EXIT THEN
   dup #trap - IF  postpone ;  EXIT THEN  drop
   T exit H   postpone [
   there swap - [ 1 trap-addr ] Literal u>
   IF  cr ." TRAP vector definition longer than "
       [ 1 trap-addr ] Literal . ." bytes."
   THEN   Tcp !
; immediate

: noexit     ( -- )   prev@ exit? IF  -1 tallot  THEN ;

\ ----------------------------------------------------------------------
\ Commands are always searched first during target debugging
\ ----------------------------------------------------------------------
Forth definitions

Vocabulary Command

Command get-context Constant debugger-wordlist Forth

\ ----------------------------------------------------------------------
\ the host compiler
\ ----------------------------------------------------------------------

: unknown ( addr len -- )  Method unknown ;

: host-classes ( addr len -- addr len | rdrop )
   2dup find-methods ?dup 0= ?EXIT   rdrop nip nip
   comp? IF  name>comp  ELSE  name>int  THEN  execute
;
: host-find ( addr len -- addr len | rdrop )
   2dup find-name ?dup 0= ?EXIT   rdrop nip nip
   comp? IF  name>comp  ELSE  name>int  THEN   execute
;
: host-number  ( addr len -- addr len | rdrop )
   2dup 2>r integer? ?dup 0= IF  2r> EXIT THEN  2rdrop rdrop
   comp? IF  0> IF  swap postpone Literal  THEN  postpone Literal  EXIT THEN
   drop
;
: host-compiler   ( addr len -- )
   Class-context @ IF  host-classes ." Method " unknown  EXIT THEN
   host-find host-number unknown
;
\ ----------------------------------------------------------------------
\ the target compiler
\ ----------------------------------------------------------------------

: target-classes ( addr len -- addr len | rdrop )
   2dup find-methods ?dup 0= ?EXIT
   rdrop nip nip   name>int execute
;
: debugger-find  ( addr len -- addr len | rdrop )
   dbg? IF  2dup debugger-wordlist search-wordlist
            IF  rdrop nip nip   execute  EXIT THEN
        THEN
;
: target-find   ( addr len -- addr len | rdrop )
   2dup find-name ?dup 0= ?EXIT  rdrop nip nip   name>int execute
;
: d>target  ( d.host -- d.target )
   over >r  host-target 0 ?DO  d2*  LOOP nip r> #datamask and swap
;
: target-number ( addr len -- addr len | rdrop )
   2dup 2>r integer? ?dup 0= IF  2r> EXIT THEN  2rdrop rdrop
   comp? IF  0> IF  d>target swap lit,  THEN  lit,  EXIT THEN
   dbg?  IF  0> IF  d>target swap >t    THEN  >t    EXIT THEN
   drop
;
: target-compiler ( addr len -- )
   Class-context @ IF  target-classes ." Method " unknown  EXIT THEN
   debugger-find target-find target-number unknown
;
gforth_062 [IF]

   : host-compile ( -- )
      ['] host-compiler   dup 'interpreter !   dup 'compiler !   IS parser
      Targeting off  Only Forth also
   ;
   ' host-compiler   dup 'interpreter !   dup 'compiler !   IS parser
   
   : target-compile  ( -- )
      ['] target-compiler   dup 'interpreter !   dup 'compiler !  IS parser
      Targeting on   Only Target also
   ;

[THEN] gforth_072 gforth_079 or [IF]

   : host-compiler-xt  ( addr len -- xt )  host-compiler ['] noop ;

   : host-compile ( -- )
      ['] host-compiler-xt 'interpreter !
      ['] host-compiler-xt 'compiler !
      ['] host-compiler-xt IS parser1
      Targeting off  Only Forth also
   ;
   ' host-compiler-xt 'interpreter !
   ' host-compiler-xt 'compiler !
   ' host-compiler-xt IS parser1

   : target-compiler-xt ( addr len -- xt )  target-compiler ['] noop ;

   : target-compile  ( -- )
      ['] target-compiler-xt 'interpreter !
      ['] target-compiler-xt 'compiler !
      ['] target-compiler-xt IS parser1
      Targeting on   Only Target also
   ;

[THEN] ( gforth_079 ) 0 [IF]

   : host-compile ( -- )
      default-recognizer TO forth-recognizer
      Targeting off  Only Forth also
   ;

   :noname name>int execute-;s ;
   dup
   ' lit,
   rectype: rectype-target

   : rec-debugger ( addr u -- nt rectype | rectype-null )
      dbg? IF  debugger-wordlist find-name-in dup IF  rectype-name  EXIT THEN  dup
      THEN  2drop   rectype-null
   ;
   : rec-target ( addr u -- nt rectype | rectype-null )
      find-name ?dup IF  rectype-target  EXIT THEN  rectype-null
   ;

   ' noop
   :noname  ( n -- )  comp? IF  lit,  EXIT THEN  dbg? IF  >t  EXIT THEN  drop ;
   dup
   rectype: rectype-tnum
   
   ' noop
   :noname ( d -- )  d>target swap
      comp? IF  lit, lit,  EXIT THEN
      dbg?  IF  >t >t  EXIT THEN ;
   dup
   rectype: rectype-tdnum
   
   : rec-tnum ( addr u -- n/d table | rectype-null )
      integer? ?dup 0= IF  rectype-null  EXIT THEN
      0> IF  rectype-tdnum  EXIT THEN  rectype-tnum
   ;
   $Variable target-recognizer
   align here target-recognizer !
   3 cells , ' rec-tnum A, ' rec-target A, ' rec-debugger A,
   
   : target-compile  ( -- )
      target-recognizer TO forth-recognizer
      Targeting on   Only Target also
   ;

[THEN]
\ ----------------------------------------------------------------------
\ included files
\ ----------------------------------------------------------------------
Forth definitions

include library.fs
include messages.fs  \ Definition of warnings and errors
include disasm.fs
include objects.fs
include images.fs    \ object code output files

\ ----------------------------------------------------------------------
\ Various target defining words
\ ----------------------------------------------------------------------

Variable @create  @create off
Variable Init-status

: Hcreate  ( -- )
   ?exec   Tdp @ Constant
   here Doers @ , Doers !
   uninitialized? dup IF  T initialized H  THEN  Init-status !
   [ here cell+ @create ! ] 0 T , H
Does> @       ( -- addr )
   comp? IF  lit,   op_DOES t,  EXIT THEN
    dbg? IF  >t s" \does" evaluate   THEN
;
: ;  ( # -- )
   #host case? IF  target-compile 0  THEN   postpone ;
; immediate

Target definitions Forth

: Host:   ( <name> -- # )                          ?exec Host: host-compile ;

: Macro:  ( <name> -- context addr xt :noname # )  ?exec Macro: host-compile ;

: Create  ( # -- # )
   comp? IF  postpone Hcreate  EXIT THEN
   Tcreate   class-def? IF  Last-class @ ,  THEN
; immediate

: Does>   ( #host -- #colon )
   Init-status @ IF  T uninitialized H  THEN
   postpone ;  Tcp @ @create @ ! ] #colon
; immediate

: :     ( <name> -- # )   ?exec
   Trash Countfield !   >in @ >r   have ?dup
   IF  dup cell+ @ doGoto =                                        \ is it a forward reference?
       IF  doColon over cell+ !                                    \ convert a goto into a colon definition
           >body   dup cell+ >r   dup @ there rot !                \ patch the real target address
           BEGIN  dup link@ swap resolve-goto ?dup 0= UNTIL        \ resolve all forward references
           Labels BEGIN  dup @ r@ - WHILE  @  REPEAT  r@ @ swap !  \ link word out of LABELS list
           Colons @ r@ !   r> Colons !                             \ and link it into COLONS list
           rdrop   if-prefix   ]   #colon
       EXIT THEN
       dup cell+ @ doLibdef =                                      \ is it a library definition?
       Current @ cell- @ doClass - and                             \ but not during class definitions
       IF  \ cr ." Libload: "  dup >name .name
           dup >name ?dup IF  cell+ dup   dup @ swap Countfield 2!   off  THEN  \ hide name to make redefinitions possible
           dup Libload cell+   there over cell+  ! !
           r> >in !   advance-libsource   name 2drop
           if-prefix   ]   #lib                                    \ leaves #lib for ";" to recognize
       EXIT THEN
       drop
   THEN   r> >in !
   mark-target   Create-hide   there ,
   here Colons @ , Colons !
   |  if-prefix ] #colon                                           \ leaves #colon for ";" to recognize
Does> ( -- ) [ here (doColon ! ]  Method   @
   comp? IF  source> swap #docall <resolve T JSR H  EXIT THEN
   ?dbg t_execute
;
: init:  ( <name> -- # )   T : H here Init-link @ , Init-link ! ;

: new      ( -- )   \ Initialize cross-compiler for another compilation run
   cr ." microCross version " .version ." by ks, gforth port and debugger by uho"
   Memory [ #datamask #maxprog umin 1+ cells ] Literal erase
   Data #maxdata cells erase   Initials 3 cells erase
   Tcp off  Tdp off  if-prefix  Colons off   Sequential off
   Labels off   Macro off   Init-link off
   Libload off   Trash Countfield !
   [ Macro#     @ ] Literal Macro# !
   [ Macro-link @ ] Literal Macro-link !
   [ Branches   @ ] Literal Branches !
   [ Operators  @ ] Literal Operators !
   [ Constants  @ ] Literal Constants !
   [ Variables  @ ] Literal Variables !
   [ Doers      @ ] Literal Doers !
;
: end  ( -- )    \ Finish target compilation run
   s" Label Initialization" evaluate
   save-data   host-compile definitions   temp-decimal
   cr there . ." instructions compiled
;
: Host   ( -- )  host-compile definitions ;

\ ----------------------------------------------------------------------
\ Redefining Target to start compilation in the target area.
\ ----------------------------------------------------------------------
Forth definitions

: Target   ( -- )
   Libfile @ IF  1 throw  ExIT THEN  
   target-compile definitions
;
\ ----------------------------------------------------------------------
\ Root vocabulary definitions.
\ To be extended when the need arises
\ ----------------------------------------------------------------------
Root definitions Forth

: t'    ( <name> -- caddr )   Context save
   postpone T defined 0= ?missing >body @
;
: [t']  ( <name> -- caddr )   t' postpone Literal ; immediate

' '           Alias h'
T h' Host H   Alias Host
' Target      Alias Target
' Only        Alias Only
' definitions Alias definitions
' see         Alias see
' .s          Alias h.s

\ ----------------------------------------------------------------------
\ Importing architecture_pkg constants into the target context
\ ----------------------------------------------------------------------
T definitions

H SIMULATION            T Version SIMULATION     \ simulating?
H EXTENDED              T Version EXTENDED       \ extended instruction set?
H WITH_MULT             T Version WITH_MULT      \ hardware multiply available?
H WITH_FLOAT            T Version WITH_FLOAT
H WITH_UP_DOWNLOAD      T Version WITH_UP_DOWNLOAD
H data_addr_width
  cache_addr_width u>   T Version WITH_EXTMEM

H data_width            T Constant data_width
H ram_data_width        T Constant ram_data_width
H data_addr_width       T Constant data_addr_width
H cache_size            T Constant cache_size
H cache_addr_width      T Constant cache_addr_width
H exp_width             T Constant exp_width
H prog_size             T Constant prog_size
H prog_addr_width       T Constant prog_addr_width
H ticks_per_ms          T Constant ticks_per_ms
H rs_addr_width         T Constant rs_addr_width
H ds_addr_width         T Constant ds_addr_width
H tasks_addr_width      T Constant tasks_addr_width
H #signbit              T Constant #signbit
H #bytes/cell           T Constant #bytes/cell

\ umbilical control characters
H mark_start            T Constant mark_start
H mark_reset            T Constant mark_reset
H mark_debug            T Constant mark_debug
H mark_ack              T Constant mark_ack
H mark_nack             T Constant mark_nack
H mark_break            T Constant mark_break
H mark_nbreak           T Constant mark_nbreak
H mark_upload           T Constant mark_upload
H mark_download         T Constant mark_download

\ memory areas
H tasks_addr_width     2** T Constant #tasks
H rs_addr_width        2** T Constant #rs-depth
H ds_addr_width        2** T Constant #ds-depth
H cache_addr_width     2** T Constant #extern       \ first address of external memory size
H addr_rstack              T Constant #rstack       \ first address used for the return stack
#rstack H cache_size umin  T Constant #cache        \ first address past internal data memory, starting at 0
#rstack #tasks #rs-depth * + Constant #rstack-end   \ first address past rstack area

\ trap vectors used, to be extended as needed.
0                          T Constant #reset        \ reset vector
1                          T Constant #isr          \ interrupt service routine
H op_PAUSE #usrmask and    T Constant #psr          \ pause service routine
H op_BREAK #usrmask and    T Constant #break        \ debugger breakpoint routine
H op_DOES  #usrmask and    T Constant #does>        \ the DOES> runtime primitive
H op_DATA  #usrmask and    T Constant #data!        \ for data memory initialization

