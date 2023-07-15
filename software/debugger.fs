\ ----------------------------------------------------------------------
\ @file : debugger.fs
\ ----------------------------------------------------------------------
\
\ Last change: KS 14.07.2023 19:23:26
\ @project: microForth/microCore
\ @language: gforth_0.6.2
\ @copyright (c): Free Software Foundation
\ @original author: uho - Ulrich Hoffmann
\ @contributor: ks - Klaus Schleisiek
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
\ @brief : microCore debuger reimplemented in Forth based on ideas of
\          the microCore debugger written in C by Andrej Kostrov.
\          This is the microCore single step debugger and tracer.
\          The target microCore system is connected via a umbilical
\          link to the host system. A communications protocol allows
\          the transfer of execution tokens (addresses) to the target
\          where a tiny debug interpreter executes these tokens.
\          See file monitor.fs for target side details.
\
\ Version Author   Date       Changes
\          uho   14-Feb-2009  initial port to gforth_062
\          uho   25-Mar-2009  early support for single stepping
\          uho    1-Apr-2009  page structure in source code, enhanced
\                             communication control flow, interpreter in
\                             single step mode, watch points with multiple
\                             target memory cells, debug interpreter with
\                             search order
\          uho   11-Apr-2009  Breakpoints, nest, end-trace
\          uho   21-Jun-2009  added abort to stop debugging immediately
\  2300     ks   04-Feb-2021  #octetts moved to microcross.fs
\  2400     ks   03-Nov-2022  byte addressing using byte_addr_width
\ ----------------------------------------------------------------------
Host

include umbilical.fs  \ defines the actual serial port used on this computer
include monitor.fs    \ debugger support in the target system

\ ----------------------------------------------------------------------
\ Table data structure
\ ----------------------------------------------------------------------
Host

: Table: ( size itemsize -- ) \ { len | size | len*x }
    Create 0 , dup , cells allot ;

: table-length ( table -- len ) @ ;
: table-size ( table -- size ) cell+ @ ;
: table-data ( table -- 'data )  2 cells + ; 
: table-clear ( table -- )  off ;
: table-bounds ( table -- to from ) 
    dup table-data swap table-length cells  bounds ;

: table-iterate ( i*x xt table -- j*x )
   table-bounds
   ?DO ( i*x xt )
      I @  swap  dup >r  execute  r>
   cell +LOOP ( j*x xt )
   drop
;
: table-2iterate ( i*x xt table -- j*x )
   table-bounds
   ?DO ( i*x xt )
      I 2@  rot  dup >r  execute  r>
   2 cells +LOOP ( j*x xt )
   drop
;
\ : table-show ( table -- )
\    ['] . swap table-iterate ;

\ ----------------------------------------------------------------------
\ ... append, remove
\ ----------------------------------------------------------------------
    
: table-append ( item table -- )
   dup table-length 1+  over
   table-size over < Abort" Table exhausted!"   ( item table length )
   over !                                       ( item table )
   dup @ cells + cell+ ! ;

: >table-item ( i table -- addr )
   2dup table-length u< 0= Abort" Table index out of range"
   2 cells + swap cells + ;

: table-item ( i table -- x )  >table-item @ ;

: table-remove ( i table -- )
    dup >r
    2dup table-length swap - 1-  ( rest ) >r
    >table-item dup cell+ swap ( from to )
    r> cells cmove
    -1 r> +! ;

: table-pop ( table -- x )
    dup table-length 1- swap  2dup table-item >r table-remove r> ;

: table-drop ( table -- )  table-pop drop ;

\ ----------------------------------------------------------------------
\ ... find items
\ ----------------------------------------------------------------------

: table-find ( item table -- i tf | ff )
    dup table-length 0
    ?DO ( item table )
       2dup I swap table-item  = IF  2drop I true UNLOOP EXIT THEN
    LOOP ( item table )
    2drop false ;

: table-2find ( item table -- i tf | ff )
    dup table-length 0
    ?DO ( item table )
       2dup I swap table-item  = IF  2drop I true UNLOOP EXIT THEN
    2 +LOOP ( item table )
    2drop false ;

\ ----------------------------------------------------------------------
\ ... dynamic tables
\ ----------------------------------------------------------------------

\ : allocate-table ( n -- table )  
\    dup 2 + cells allocate throw  
\    dup off   swap over cell+  ! ;

\ : free-table ( table -- )  free throw ;

\ ----------------------------------------------------------------------
\ Debugger communication
\ ----------------------------------------------------------------------

Variable Umbilical   Umbilical off

: .hex## ( x -- )  temp-hex dup <# # # #> type ;

: tx ( x -- )   Umbilical @
   IF dup mark_ack = IF ."  txack " ELSE cr ." tx " dup .hex## THEN
   THEN   term-emit term-flush
;
: rx ( -- x )  term-key Umbilical @ 0= ?EXIT
   dup mark_ack = IF ."  rxack " ELSE cr ." rx " dup .hex## THEN
;
: rx? ( -- flag )  term-key? ;

: rxflush ( -- )
   rx? IF  cr ." Flush: "  THEN
   BEGIN  rx? WHILE  term-key .hex## space  REPEAT
;
\ ----------------------------------------------------------------------
\ ... send, receive and log data
\ ----------------------------------------------------------------------

: log-cr ( -- )   Umbilical @ IF cr THEN ;

: log-string ( addr len -- )
   Umbilical @ IF type ELSE 2drop THEN
;

data_width 4 /mod swap 0<> - Constant #hex/word

: log-word ( x -- )
   Umbilical @ 0= IF  drop EXIT THEN  space
   temp-hex dup <# #hex/word 0 DO # LOOP #> type
;
: transmit ( x -- )
   dup log-word
   0 #octetts 1- ?DO  dup I 8 * rshift term-emit  -1 +LOOP  drop
   term-flush
;
: receive ( -- x )
   0   #octetts 0 ?DO  8 lshift term-key or  LOOP  dup log-word
;
byte_addr_width [IF]
   : ctransmit  ( c -- )   dup log-word  term-emit term-flush ;

   : creceive   ( -- c )   term-key dup log-word ;
[THEN]
\ ----------------------------------------------------------------------
\ ... target communications protocol
\ ----------------------------------------------------------------------

: ack ( -- )   mark_ack  tx ;
: nack ( -- )  mark_nack tx ;
: *nack ( -- ) umbilical @ umbilical off #octetts 0 ?DO nack LOOP umbilical ! ;

 &18 Constant #nack-detected
 &19 Constant #protocol-error
 &20 Constant #handshake-error
  -1 Constant #abort
-&28 Constant #bye

: ?ack ( -- )   rx
   mark_ack  case? ?EXIT
   mark_nack case? IF  #nack-detected throw  THEN
   space .hex## ."  was unexpected " #protocol-error throw
;
\ ----------------------------------------------------------------------
\ ... handshake
\ ----------------------------------------------------------------------

Variable Broken  Broken off   \ break issued to Target as a command when ON

: handshake ( -- )
   Broken @ IF  mark_nbreak tx   &10 ms   Broken off  THEN
   rxflush ."  H"
   BEGIN  0 >target  ." A"  target> #warmboot = UNTIL
   BEGIN  ." N"
      rx? 0= IF $3F5 >target  THEN \ shorten replies if target still sends data
      ." D" target> $305 -
   WHILE
      key? IF  break-key? IF  #handshake-error throw  THEN THEN
   REPEAT
   ." S"  0 >target
   ." H"  target>
   ." A"  ?dup IF ." Handshake error. 0 expected, " $. ." received"
                  #handshake-error throw
          THEN
   ." KE "
;
: do-handshake ( -- ) ." , " handshake ." done" ;

: imgtx ( x -- )   Umbilical @ IF dup space .hex## THEN  term-emit ;

:noname ( t-addr len cpuInit -- )
   *nack  rxflush
   IF  mark_reset  ELSE  mark_start  THEN tx
   over transmit  \ t-addr
   dup  transmit  \ len
   bounds ?DO  I opcode@ imgtx  LOOP
   term-flush   ?ack
; IS send-image

: boot-image ( -- )
    0 there  true send-image  there Transferred ! ;

\ ----------------------------------------------------------------------
\ ... upload program image and up/downmove data memory areas
\ followed by up/download files, one number/line according to base
\ ----------------------------------------------------------------------

WITH_UP_DOWNLOAD [IF]
   byte_addr_width [IF] \ byte addressing

      : upmove  ( from.host to.target #bytes -- )
         mark_upload tx   swap transmit   dup transmit  \ from.host bytes
         bounds ?DO  I c@ ctransmit  LOOP   ?ack
      ;
      : downmove  ( from.target to.host #bytes -- )
         mark_download tx   rot transmit   dup transmit  \ to.host bytes
         bounds ?DO  creceive I c!  LOOP   ack
      ;
      : write_number   ( n pid -- )
         >r   dup 0< tuck IF  abs  THEN
         0 <# #s [char] $ hold rot sign #>
         r> write-line abort" file write error"
      ;
      : move>file  ( <filename> addr len -- )
         depth 2 u< abort" Target addr len needed."
         BL word dup c@ 0= abort" Destination file name required"
         count R/W create-file abort" File could not be created" -rot  \ pid addr len
         bounds DO  I c@ over write_number  LOOP
         close-file abort" File close failed"
      ;
      : download  ( from.target #cells <filename> -- )
         temp-hex   >r pad r@ downmove   pad r> move>file
      ;
      : read_number  ( pid -- n tf | ff )
         here [ data_width 2 + ] Literal rot read-line abort" read line failed"
         IF  here swap s>number drop true  ELSE  drop false  THEN
      ;
      : upload  ( to.target <filename> -- )
         depth 1 u< abort" Target addr needed."
         BL word dup c@ 0= abort" Source file name required"
         count R/O open-file abort" File not found" >r
         pad BEGIN  r@ read_number WHILE  over c!  1+  REPEAT
         r> close-file abort" File close failed"
         pad -   pad -rot upmove
      ;

   [ELSE] \ cell addressing

      : upmove  ( from.host to.target #cells -- )
         mark_upload tx   swap transmit   dup transmit  \ from.host cells
         cells bounds ?DO  I @ transmit   cell +LOOP   ?ack
      ;
      : downmove  ( from.target to.host #cells -- )
         mark_download tx   rot transmit   dup transmit  \ to.host cells
         cells bounds ?DO  receive ?signed I !  cell +LOOP  ack
      ;
      : write_number   ( n pid -- )
         >r   dup 0< tuck IF  abs  THEN
         0 <# #s [char] $ hold rot sign #>
         r> write-line abort" file write error"
      ;
      : move>file  ( <filename> addr len -- )
         depth 2 u< abort" Target addr len needed."
         BL word dup c@ 0= abort" Destination file name required"
         count R/W create-file abort" File could not be created" -rot  \ pid addr len
         cells bounds DO  I @ over write_number  cell +LOOP
         close-file abort" File close failed"
      ;
      : download  ( from.target #cells <filename> -- )
         temp-hex   >r pad r@ downmove   pad r> move>file
      ;
      : read_number  ( pid -- n tf | ff )
         here [ data_width 2 + ] Literal rot read-line abort" read line failed"
         IF  here swap s>number drop true  ELSE  drop false  THEN
      ;
      : upload  ( to.target <filename> -- )
         depth 1 u< abort" Target addr needed."
         BL word dup c@ 0= abort" Source file name required"
         count R/O open-file abort" File not found" >r
         pad BEGIN  r@ read_number WHILE  over !  cell+  REPEAT
         r> close-file abort" File close failed"
         pad - cell /   pad -rot upmove
      ;
   [THEN]
[THEN]
\ ----------------------------------------------------------------------
\ ... setting breakpoints
\ ----------------------------------------------------------------------

: set-breakpoint ( addr -- )   
   *nack
   mark_start tx    
   transmit     \ t-addr
   1 transmit   \ len
   op_BREAK tx
   ?ack
;
: restore-instruction ( addr -- )
   *nack
   mark_start tx    
   dup transmit  \ t-addr
   1 transmit    \ len
   >memory @ tx 
   ?ack
;
\ ----------------------------------------------------------------------
\ handling target messages
\ ----------------------------------------------------------------------

Defer do-handle-breakpoint

: ?OK ( -- )
   BEGIN 
      target> ?dup 0= ?EXIT
      #warmboot   case? IF ." uCore reset" handshake  EXIT THEN
      #breakpoint case? IF target>
                           target> IF  #protocol-error throw  THEN
                           do-handle-breakpoint
                        ELSE
                           do-messages
                        THEN
   AGAIN
;
\ ----------------------------------------------------------------------
\ send addresses and literals
\ ----------------------------------------------------------------------

:noname  ( -- x )       [t'] \host! >target target> ?OK ;
IS t>

:noname  ( x -- )       [t'] \host@ >target  >target ?OK ;
IS >t

: t_@    ( addr -- x )  >t  [t'] \@ t_execute  t> ;

: t_2@   ( addr -- d )  dup 1+ t_@ swap t_@ ;

: t_!    ( x addr -- )  swap >t >t  [t'] \! t_execute ;

: t_2!   ( d addr -- )  dup >r t_!  r> 1+ t_! ;

:noname  ( xt -- )      >target  ?OK ;
IS t_execute

:noname  ( x -- )       mark_debug tx transmit ?ack ;
IS >target

:noname  ( -- x )       BEGIN  rx mark_debug = UNTIL  receive ack ;
IS target>

byte_addr_width [IF]
: t_c@   ( caddr -- c ) >t [t'] \c@ t_execute t> ;
[THEN]
\ ----------------------------------------------------------------------
\ permanent and temporary breakpoints
\ ----------------------------------------------------------------------

: install-breakpoint ( taddr table -- )
\   Verbose @ IF ."  installing breakpoint " over $. THEN
   over set-breakpoint
   2dup table-find IF  drop 2drop  EXIT THEN
   table-append
;
: remove-breakpoint ( taddr table -- ) 
\   Verbose @ IF ."  removing breakpoint " over $. THEN
   over restore-instruction
   swap over  table-find IF swap table-remove ELSE drop THEN
;
: remove-all-breakpoints ( table -- )
   BEGIN  dup table-length
   WHILE  0 over table-item  over remove-breakpoint
   REPEAT  drop
;
: set-all-breakpoints ( table -- )  ['] set-breakpoint swap table-iterate ;

&32 Table: permanent-breakpoints
 &8 Table: temporary-breakpoints

: install-temporary-breakpoint ( taddr -- )
   temporary-breakpoints install-breakpoint
;
: permanent-breakpoint? ( taddr -- f ) 
   permanent-breakpoints table-find dup IF nip THEN
;
: .breakpoint ( taddr -- ) Colons .listname ;

\ ----------------------------------------------------------------------
\ move temporary breakpoints
\ ----------------------------------------------------------------------

&32 Table: nests

: -nest ( -- )  nests table-length 0= ?EXIT
   nests table-pop install-temporary-breakpoint
;
: target-address ( addr -- addr' )
   dup nibbles@ nibbles>   swap >branch drop  + 1+
;
: advance-breakpoint ( addr nextaddr -- )
   over opcode@                               ( oldaddr newaddr opcode )
   op_EXIT   case? IF  -nest 2drop  EXIT THEN
   op_IRET   case? IF  -nest 2drop  EXIT THEN
   op_NZEXIT case? IF  t> dup >t    IF  -nest 2drop EXIT THEN  op_NZEXIT  THEN
   dup #literal and
   IF drop                                    ( oldaddr newaddr )
      over >branch drop opcode@ a-branch? IF  ( oldaddr newaddr )
         dup install-temporary-breakpoint                       \ after jump
         swap nibbles@ nibbles> + install-temporary-breakpoint  \ at jump target
      EXIT THEN                               ( oldaddr newaddr )
      nip install-temporary-breakpoint                          \ after literal
   EXIT THEN
   \ sequential execution
   drop nip install-temporary-breakpoint                        \ after instruction
;
\ ----------------------------------------------------------------------
\ watchpoints
\ ----------------------------------------------------------------------

&32 Table: watches

: .watch ( n xt -- )
    cr swap >r dup >name ."    " .name ." = " >body @
    r> bounds ?DO I t_@ signed? IF  signextend . ELSE  tu.  THEN  LOOP 
;
: .watches ( -- )  ['] .watch watches table-2iterate ;

: add-watch ( addr n -- )  swap  watches table-append  watches table-append ;

: find-watch ( addr -- )  watches table-2find ;

: remove-watch ( i -- )   dup watches table-remove  watches table-remove ;
  
\ ----------------------------------------------------------------------
\ debug-interpreter
\ ----------------------------------------------------------------------

T get-context H Constant target-wordlist

: s' ( <name> -- host-xt )  \ ' symbol table
   name target-wordlist search-wordlist 0= IF #unknown throw THEN
;
\ ----------------------------------------------------------------------
\ ... main loop
\ ----------------------------------------------------------------------
  
gforth_062 [IF]

: debugger-interpret ( -- )   BEGIN name dup WHILE target-compiler REPEAT 2drop ;

[THEN] gforth_072 gforth_079 or [IF]

: debugger-interpret ( -- )   BEGIN name dup WHILE target-compiler drop REPEAT 2drop ;

[THEN]
   
: input ( -- ) \ get a line to tib
   \ similar to query, but gforth query has problems with a partially 
   \ initialized system try  gforth-0.6.2  -e query  and do some input 
   \ to see the address violation
   tib /line accept #tib ! >in off
;
: (debugger ( -- )
   .watches
   permanent-breakpoints set-all-breakpoints
   temporary-breakpoints remove-all-breakpoints
   cr ." uCore> " input debugger-interpret
;
: handle-debug-error ( n -- )
   #unknown case? IF  ." ?"  EXIT THEN
   #nack-detected  case? IF ." nack from target" do-handshake  EXIT THEN
   #protocol-error case? IF
      ." garbage from target, try pressing reset on target"
      rxflush do-handshake
   EXIT THEN
   ( ." Error " . ) doerror
;
\ ----------------------------------------------------------------------
\ ... top level command
\ ----------------------------------------------------------------------

: .target ( -- )  ;

: .host   ( -- )  ."  Back in the host " ;

: debugger ( -- )  \ umbilical on
   Broken @ IF  handshake  THEN
   .target   Warnings on   Target   Debugging on   nests table-clear
   BEGIN
       ['] (debugger catch ?dup
       IF #bye case? IF  .host Warnings off   Debugging off   Host  EXIT THEN
          handle-debug-error
       ELSE
          comp? IF ." ]"
          ELSE ." ok"  class-context @ 0=
             IF  s" have Dp" evaluate  \ keep Tdp and Dp in the Target (when it exists!) synchronized
                 IF  s" Dp @" evaluate t> Tdp @ max dup Tdp ! >t s" Dp !" evaluate  THEN
             THEN
          THEN
       THEN
   AGAIN
;
' debugger Alias ucore
' debugger Alias dbg

\ ----------------------------------------------------------------------
\ ... breakpoint action
\ ----------------------------------------------------------------------

: show-dstack ( -- )
   [t'] copyds >target
   target>  ( depth )  ." <" dup 0 u.r ." > "
   0 ?DO  target> signed? IF signextend . ELSE tu. THEN LOOP
   ?OK
;
: show-breakpoint ( addr -- addr' )   .instruction space   &40 position show-dstack ;

: another-command? ( -- f )  #tib @ ;

: trigger-resume   ( -- )    #tib off ;

: interpret-breakpoint-commands ( addr nextaddr -- addr' nextaddr' )
   ['] debugger-interpret catch
          0 case? IF another-command? 0= ?EXIT  ." ok" EXIT THEN
   #unknown case? IF ." ?"  EXIT THEN
   #abort   case? IF ." aborted" EXIT THEN
   throw
;
\ ----------------------------------------------------------------------
\ ... breakpoint handler
\ ----------------------------------------------------------------------

: breakpoint-prompt ( -- )  space nests table-length 1+ 0 DO ." >" LOOP space ;

Variable temp-TOR

: handle-breakpoint ( addr -- )
   temporary-breakpoints remove-all-breakpoints
   [t'] saveTOR t_execute
   BEGIN                                                   ( addr )
      dis-output   dup show-breakpoint                     ( addr nextaddr )
      &60 position breakpoint-prompt  input
      std-output
      #tib @ IF  interpret-breakpoint-commands   THEN      ( addr nextaddr )
      another-command?
   WHILE  drop
   REPEAT                                                  ( addr nextaddr )
   [t'] restoreTOR t_execute
   over swap 2dup -
   IF  over restore-instruction   advance-breakpoint
   ELSE  2drop
   THEN                                                    ( addr )
\   Verbose @ IF ."  resuming at " dup $. THEN
   [t'] nextstep >target  >target
;
' handle-breakpoint is do-handle-breakpoint

\ ----------------------------------------------------------------------
\ special words available in debugger
\ ----------------------------------------------------------------------

: addr.   ( caddr -- )   [ data_width 3 + 4 / ] Literal u.r ;

: signed. ( n -- )       ?signed [ data_width 3 + 4 / ] Literal .r ;

Command definitions

: reset ( -- )  0 0 true send-image &20 ms handshake ;

: bye   ( -- )  #bye throw ;

: break ( -- )  mark_break tx   Broken on   #bye throw ;

: commands ( -- ) get-order Command words  set-order ;

: help ( -- ) commands ;

: disasm ( -- ) ( T addr -- ) t> disasm ;

: show ( <name> -- ) show ;

: dump  ( -- )  ( T addr len -- )
   t> tcells t> swap bounds
   ?DO  cr I .addr
        I 8 tcells bounds DO  I t_@ signed. space  #cell +LOOP
   8 tcells +LOOP
;
: udump  ( -- )  ( T addr len -- )
   temp-hex   t> tcells t> swap bounds
   ?DO  cr I .addr
        I 8 tcells bounds DO  I t_@ addr. space  #cell +LOOP
   8 tcells +LOOP
;
byte_addr_width [IF]

: cdump  ( -- ) ( T caddr len -- )
   temp-hex   t> t> swap bounds
   ?DO  cr I .addr
        I $20 bounds DO  I t_c@ 2 u.r space  LOOP
   $20 +LOOP
;
[THEN]

: ' ( <name> -- )  ( T -- xt )  t' >t ;

WITH_UP_DOWNLOAD [IF]

   : download  ( from.target quan <filename> -- )  t> t> swap download ;

   : upload    ( to.target <filename> -- )         t> upload ;

[THEN]
\ ----------------------------------------------------------------------
\ ... times
\ ----------------------------------------------------------------------
Host

Variable times-count  times-count off

Command definitions

: times   ( n -- )
   t> ?dup IF  times-count @ 1 + dup times-count !
       1 +  u<  stop? or IF  times-count off  EXIT THEN
   ELSE  stop? ?EXIT
   THEN
   >in off
;
\ ----------------------------------------------------------------------
\ ... watchpoints
\ ----------------------------------------------------------------------

: #watch ( n <name> -- )  s' t> add-watch ;

: watch ( <name> -- )  s'  1 add-watch ;

: unwatch ( <name> -- ) 
    s'  find-watch IF remove-watch EXIT THEN 
    ." is not watched " ;

: unwatch-all ( -- )  watches table-clear ;

\ ----------------------------------------------------------------------
\ ... debug trace end-trace
\ ----------------------------------------------------------------------

: breakpoint  ( taddr -- )  t> permanent-breakpoints install-breakpoint ;

: debug ( <name> -- )   s' >body @  permanent-breakpoints install-breakpoint ;

: unbug ( <name> -- )   s' >body @  permanent-breakpoints remove-breakpoint ;

: breakpoints ( -- )  
   2 spaces  ['] .breakpoint permanent-breakpoints table-iterate ;

: unbug-all ( -- ) permanent-breakpoints remove-all-breakpoints ;

: trace ( <name> -- )  s' >body @ dup set-breakpoint t_execute ;

: nest ( H addr nextaddr -- addr nextaddr )
   over >branch drop opcode@
   call? IF  dup nests table-append
             over target-address install-temporary-breakpoint 
             trigger-resume
         EXIT THEN
   ." can nest only on calls - " abort ;

: unnest ( H addr nextaddr -- addr addr )   
   -nest
   drop dup    \ trigger skip of advance-breakpoint
   trigger-resume
;
: end-trace ( H addr nextaddr -- addr addr )
   unnest
   nests table-clear
   temporary-breakpoints remove-all-breakpoints
   unbug-all
;
\ ----------------------------------------------------------------------
\ ... jump after abort
\ ----------------------------------------------------------------------

: jump ( H addr nextaddr -- addr addr )
    \ do not execute current instruction, jump over it and break on next
    dup >r  advance-breakpoint
    r> dup  \ trigger not to automatically advance breakpoint 
    trigger-resume ;

: after ( H addr nextaddr -- addr addr )
    \ to be used at backward jump in a loop to set a breakpoint
    \ after the loop 
    install-temporary-breakpoint
    dup \ trigger not to automatically advance breakpoint 
    trigger-resume ;      

: abort ( H addr nextaddr -- addr addr )   \ immediately stop tracing
    end-trace 
    [t'] rclear t_execute
    2drop  [t'] monitor dup 
;
Host

: run     ( -- )  boot-image handshake debugger ;

: connect ( -- )  Broken on handshake debugger ;

Root definitions Forth

: .s  ( i*x -- i*x )    dbg? IF  show-dstack  EXIT THEN .s ;

\ ----------------------------------------------------------------------
\ target words, which can be executed and compiled into debug words
\ for the target system
\ ----------------------------------------------------------------------
Target SIMULATION [NOTIF]

Variable Dp   \ initialized by the word "end" (microcross.fs)
\ in addition, Dp (target) and Tdp (host) are synchronised in debugger (debugger.fs)

Host: here  ( -- addr )      comp? dbg? or IF  T Dp @         H EXIT THEN  Tdp @ ;
Host: allot ( n -- )         comp? dbg? or IF  T Dp @ + Dp !  H EXIT THEN  Tdp +! ;
    : ,     ( n -- )         here !   #cell allot ;
Host: ,     ( u -- )         comp? dbg? or IF  T , H  EXIT THEN  Tdp @ d!   #cell Tdp +! ;

Host: .     ( n -- )         comp? IF  #dot   lit, T message host!       H EXIT THEN  dbg? IF  t> signextend         THEN  . ;
Host: .r    ( n u -- )       comp? IF  #dotr  lit, T message host! host! H EXIT THEN  dbg? IF  t> t> signextend swap THEN  .r ;
Host: u.    ( u -- )         comp? IF  #udot  lit, T message host!       H EXIT THEN  dbg? IF  t>                    THEN  tu. ;
Host: d.    ( d -- )         comp? IF  #ddot  lit, T message host! host! H EXIT THEN  dbg? IF  t> t> swap dtarget    THEN  d. ;
Host: ud.   ( ud -- )        comp? IF  #uddot lit, T message host! host! H EXIT THEN  dbg? IF  t> t> swap udtarget   THEN  ud. ;
Host: cr    ( -- )           comp? IF  #cret  lit, T message             H EXIT THEN                                       cr ;
Host: emit  ( char -- )      comp? IF  #emit  lit, T message host!       H EXIT THEN  dbg? IF  t>                    THEN  emit ;
Host: 2//   ( u1 -- u2 )     ?exec                                                    dbg? IF  t> 2// >t        EXIT THEN  2// ;
Host: $.    ( u -- )         ?exec                                                    dbg? IF  t>                    THEN  $. ;
Host: &.    ( n -- )         ?exec                                                    dbg? IF  t>                    THEN  &. ;
Host: #.    ( n -- )         ?exec                                                    dbg? IF  t>                    THEN  #. ;
Host: %.    ( u -- )         ?exec                                                    dbg? IF  t>                    THEN  %. ;

[THEN]
