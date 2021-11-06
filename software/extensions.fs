\ ----------------------------------------------------------------------
\ @file : extensions.fs
\ ----------------------------------------------------------------------
\
\ Last change: KS 16.06.2021 16:49:53
\ @project: microForth/microCore
\ @language: gforth_0.6.2
\ @copyright (c): Free Software Foundation
\ @original author: ks - Klaus Schleisiek
\ @contributor: uho - Ulrich Hoffmann
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
\ @brief : Compatibility layer loads on top of gforth.
\
\ Version Author   Date       Changes
\   210     ks   14-Jun-2020  initial version
\ ----------------------------------------------------------------------
Only Forth also definitions

warnings off

version-string s" 0.6.2"        str= Constant gforth_062 immediate
version-string s" 0.7.2"        str= Constant gforth_072 immediate
version-string drop 5 s" 0.7.9" str= Constant gforth_079 immediate

gforth_062 gforth_072 or gforth_079 or 0= [IF]
cr .( gforth ) version-string type .(  not supported)  abort
[THEN]

: cell_width ( -- u )
   0 1 BEGIN  swap 1+ swap  2* ?dup 0= UNTIL
;
: cell-    1 cells - ;

' invert Alias not

: w@ ( addr -- w )   @ $ffff and ;

: mem-save ( x1 ... xn n -- addr )
   dup 1+ cells allocate Abort" mem-save: allocate memory failed"  \ x1 ... xn n addr
   dup >r   2dup ! cell+                \ store length
   swap 0 ?DO  swap over ! cell+  LOOP  ( addr1 )
   drop r>
;
: mem-restore ( addr -- x1 ... xn n )
   dup >r dup @                      ( addr len )
   dup >r cells +                    ( addr' )
   r@ 0 ?DO  dup @ swap cell-  LOOP  ( x1 ... xn addr )
   drop r>                           ( x1 ... xn n )
   r> free Abort" mem-restore: free memory failed"
;
: source>     ( -- sca )  save-input mem-save ;

: >source     ( sca -- )  mem-restore  restore-input abort" couldn't restore input" ;

: drop-source ( sca -- )  free Abort" drop-source: cannot free source code info" ;

: skip-input  ( c -- )
   source dup >in @ min /string  over >r
   rot skip drop  r> - >in +!
;
: scan-input  ( c ccccc[c] -- )
   loadfile @ 0= abort" can only be loaded from file"
   BEGIN  >in @ over parse nip
      >in @ rot - =                 \ is there no delimter?
   WHILE
      refill 0= IF  drop EXIT THEN
   REPEAT  drop
;
: token  ( addr len c -- addr' len' )  \ token delimited by c
   dup >r skip   2dup r> scan   nip -
;
: u2/     ( u1 -- u2 )     1 rshift ; \ Assumes 2's complement arithmetic
: ud2/    ( ud1 -- ud2 )   dup 1 and >r  u2/ swap u2/ 0 r> d2/ drop or swap ;
: 2**     ( u -- 2**u )    1 swap 0 ?DO 2* LOOP ;
: 2//     ( u -- log2_u )  >r 0  BEGIN  dup 2** r@ < WHILE  1+  REPEAT r> drop ;
: binary  ( -- )           2 Base ! ;
: octal   ( -- )           8 Base ! ;
: clear   ( xxx -- )       BEGIN  depth WHILE drop REPEAT ;
: rdepth  ( -- u )         rp0 @ rp@ - 1 cells / ;
: case?   ( n1 n2 -- n1 ff | tf )   over = dup IF  nip  THEN ;  \ the most primitive case operator
: ud*     ( ud u  -- udprod )       tuck um* drop >r um* r> + ;

\ ----------------------------------------------------------------------
\ Number input
\ ----------------------------------------------------------------------

Base @ hex   Create Prefixes  A , 10 , 2 , A ,   Base !
\                             #    $   %   &  base prefix character
: ?base ( caddr u -- caddr' u' )
   over c@ [char] # - dup 4 u<
   IF  cells Prefixes + @   base !  1 /string  EXIT THEN
   drop
;
: ?sign ( caddr u -- caddr' u' flag )
   over c@ [char] - = >r  r@ IF  1 /string  THEN   r>
;
: integer? ( caddr u -- 0 / n -1 / d 0> )
   Base @ -rot   0 >r   ?sign >r   ?base   0. 2swap
   BEGIN  >number ?dup 0=
      IF  drop  rot Base !   r> IF  dnegate  THEN
          r> ?dup ?EXIT      \ double number
          drop True  EXIT    \ single number
      THEN
      over c@ [char] . =
   WHILE  r> rdrop over >r >r   1 /string
   REPEAT
   2drop 2drop rdrop rdrop   Base !  False
;
\ ----------------------------------------------------------------------
\ IO Redirection
\ ----------------------------------------------------------------------

: _CR ( -- )  newline (type)  ;

Defer CR  

: _EMIT ( c -- ) (emit) ;

: _TYPE ( c-addr len -- )
    2dup newline d= IF  2drop  CR EXIT THEN  \ make old cr invoke new CR
    (type) ;

' _TYPE IS TYPE
' _CR   IS CR
' _EMIT IS EMIT

: ascii ( -- ch ) char ;

: Module ( <name> -- )
   >IN @ >R  BL WORD FIND IF  EXECUTE  ELSE DROP THEN  R> >IN !  MARKER ;

\ ----------------------------------------------------------------------
\ Parser redirection
\ ----------------------------------------------------------------------

: becomes  ( <name> new-xt -- ) \ makes <name> behave as new-xt
   >r   here   ' >body dp !
   postpone ahead   r> >body dp !   postpone THEN
   dp !
;
-&13 Constant #unknown

: unknown  ( addr u -- )  type space #unknown throw ;

gforth_062 [IF]

\   : ks-DoError ( throw-code -- )  drop
\   .error-string ."  ?" normal-dp dpp !
\   ;
\   ' ks-DoError IS DoError

   Create save[     2 cells allot   ' [    >body save[    2 cells cmove
   Create save]     2 cells allot   ' ]    >body save]    2 cells cmove
   Create save.voc  2 cells allot   ' .voc >body save.voc 2 cells cmove
   
   : unpatch  ( -- )
      save[    [ ' [    >body ] Literal 2 cells cmove
      save]    [ ' ]    >body ] Literal 2 cells cmove
      save.voc [ ' .voc >body ] Literal 2 cells cmove
      ['] no.extensions   dup IS compiler-notfound   IS interpreter-notfound
      ['] interpreter IS parser
      ['] (DoError) IS DoError
   ;
   ' unknown IS compiler-notfound
   ' unknown IS interpreter-notfound

   Variable 'interpreter   ' interpreter 'interpreter !
   Variable 'compiler      ' compiler    'compiler !

   :noname ( -- )  'interpreter @ IS parser   state off ; becomes [

   :noname ( -- )  'compiler @    IS parser   state on  ; becomes ]

   :noname ( wid -- )
      dup >r wordlist-struct %size + dup head? -1 =
      IF ( wid nt )
          dup name>int dup >code-address
          docon: = swap >body @ r@ = and
          IF  id. rdrop EXIT THEN
      THEN
      drop r> body> >head-noprim id.
   ; becomes .voc

   : Create-hide ( "name" -- )   Header dovar: cfa, ;

   ' (search-wordlist) Alias find-name-in

[THEN] gforth_072 [IF]

   Create save[     2 cells allot   ' [    >body save[    2 cells cmove
   Create save]     2 cells allot   ' ]    >body save]    2 cells cmove

   : unpatch  ( -- )
      save[ [ ' [ >body ] Literal 2 cells cmove
      save] [ ' ] >body ] Literal 2 cells cmove
      ['] interpreter1 IS parser1
      ['] (DoError) IS DoError
   ;
   Variable 'interpreter   ' interpreter1 'interpreter !
   Variable 'compiler      ' compiler1    'compiler !

   :noname ( -- )  'interpreter @ IS parser1   state off ; becomes [

   :noname ( -- )  'compiler @    IS parser1   state on  ; becomes ]

   : Create-hide ( "name" -- )   Header dovar: cfa, ;

   ' (search-wordlist) Alias find-name-in

[THEN] gforth_079 [IF]

   Create save[ 2 cells allot  ' [ >body save[ 2 cells cmove
   Create save] 2 cells allot  ' ] >body save] 2 cells cmove

   : unpatch  ( -- )
      save[ [ ' [ >body ] Literal 2 cells cmove
      save] [ ' ] >body ] Literal 2 cells cmove
      ['] interpreter-r IS parser1
      ['] (DoError) IS DoError
   ;
   Variable 'interpreter   ' interpreter-r 'interpreter !
   Variable 'compiler      ' compiler-r    'compiler !

   :noname ( -- )  'interpreter @ IS parser1   state off ; becomes [

   :noname ( -- )  'compiler @    IS parser1   state on  ; becomes ]

   : Create-hide ( "name" -- )   ['] udp create-from ;

   : ' ( "name" -- nt )  name name-too-short? find-name ?dup ?EXIT  #unknown throw ;

   ' execute IS int-execute

[THEN]

: forget ( -- ) ' drop ;

\ floored division
: /  ( n1 n2 -- n3  ) >r s>d r> fm/mod nip ;

: ?missing ( f -- ) ABORT" not found" ;

: defined ( <name> -- xxxx ff | xt tf )
   name over swap   name-too-short?
   find-name dup IF  name?int nip true  THEN
; 
: get-context  ( -- wid )   Context @ ;

: .parser  ( -- )   ['] parser >body @ >name .name ;

\ ----------------------------------------------------------------------
\ Some System word (re)definitions for a more sympathetic environment
\ ----------------------------------------------------------------------

: ?cr  ( -- ) ;

$0A Constant #lf
$0D Constant #cr

: stop? ( -- flag )
   key? IF  key #cr = ?dup ?EXIT
            key #cr = EXIT
   THEN  false
;
: .name      ( nt -- )    ?dup IF  name>string type space EXIT THEN  ." ??? " ;

: .wordname  ( pfa -- )   body> >name .name ;

: decomp  ( addr -- )   BEGIN  cr dup 6 u.r ." : " dup @ dup 6 u.r space .wordname cell+ key #cr = UNTIL drop ;

: have ( ccc -- xt | false )  BL word find 0<> and ;

: shift  ( n1 quan -- n2 )    dup 0< IF  abs rshift  EXIT THEN  lshift ;
: ashift ( n1 n2 -- n3 )      dup 0< IF  negate 0 DO 2/ LOOP EXIT THEN  0 ?DO 2* LOOP ;
: m/mod  ( d n -- rem quot )  fm/mod ;
: ndrop  ( x1 .. xn n -- )    0 ?DO  drop  LOOP ;
: pack   ( c u -- u' )        $100 um* drop swap $FF and or ;
: unpack ( u -- c u' )        0 $100 um/mod ;

Variable Debugging  Debugging off

: comp?  ( -- f )   State @ 0<> ;
: exec?  ( -- f )   State @ 0= ;
: dbg?   ( -- f )   Debugging @ exec? and ;

: ?comp  ( -- )     comp? 0= Abort" compilation only" ;
: ?exec  ( -- )     exec? 0= Abort" execution only" ;
: ?dbg   ( -- )     dbg?  0= Abort" debugging only" ;

: ?pairs  ( n1 n2 -- )   - Abort" unstructured!" ;              \ for checking conditional constructs

Create restore ] r> r> ! exit [
: save  ( var -- )  r> swap dup >r @ >r restore >r >r ;

: temp-hex     ( -- )  r> Base save hex >r ;
: temp-decimal ( -- )  r> Base save decimal >r ;
: temp-binary  ( -- )  r> Base save binary >r ;

: $.   ( u -- )  temp-hex     ." $" u. ;
: &.   ( n -- )  temp-decimal ." &" . ;
: #.   ( n -- )  temp-decimal ." #" . ;
: %.   ( u -- )  temp-binary  ." %" u. ;

$40 cell_width 4 / / Constant #items/line

: udump  ( addr u -- )  \ dumps data items as unsigned
   #items/line /mod swap 0= 1+ +
   0 ?DO  cr dup u. #Bs emit ." :"
          #items/line 0 DO  dup @ cell_width 2/ 2/ 1+ u.r cell+  LOOP
   LOOP  drop
;
: \\ ( -- )  source-id IF  BEGIN  refill 0= UNTIL  THEN  postpone \ ; immediate

: count"  ( <ccc> -- u )  [char] " word count nip ;

\ ----------------------------------------------------------------------
\ Conditional compilation, Version 3
\ ----------------------------------------------------------------------

Defer [IF]      ( flag -- )   immediate
    : [NOTIF]   ( flag -- )   0= postpone [IF] ; immediate
    : [IFDEF]   ( <name> -- ) postpone [DEFINED]  postpone [IF] ; immediate
    : [IFUNDEF] ( <name> -- ) postpone [DEFINED]  postpone [NOTIF] ; immediate
Defer [ELSE]    ( -- )        immediate
Defer [THEN]    ( -- )        immediate

: <eof> ( -- ) ; \ used to signal "end of file"

: forth-word  ( -- xt )
   BEGIN BEGIN  name ?dup
         WHILE  forth-wordlist search-wordlist ?EXIT
         REPEAT
         drop   refill 0=
   UNTIL  ['] <eof>
;
: *\   ( -- )  ; \ used to signal "end of multi-line comment"

: \*   ( -- )   BEGIN  forth-word dup ['] <eof> = swap ['] *\ = or UNTIL ; immediate

Variable Level   0 Level !

: level-1  ( -- )  Level @ 1 - 0 max Level ! ;

: [if]-decode  ( xt -- flag )
   ['] [IF]      case? IF  1 Level +!     false  EXIT THEN
   ['] [NOTIF]   case? IF  1 Level +!     false  EXIT THEN
   ['] [IFDEF]   case? IF  1 Level +!     false  EXIT THEN
   ['] [IFUNDEF] case? IF  1 Level +!     false  EXIT THEN
   ['] [ELSE]    case? IF  Level @ 0=            EXIT THEN
   ['] [THEN]    case? IF  Level @ 0=   level-1  EXIT THEN
   ['] \         case? IF  postpone \     false  EXIT THEN  \ needed to be able to e.g. comment out [THEN]
   ['] (         case? IF  postpone (     false  EXIT THEN  \ needed to be able to e.g. comment out [THEN]
   ['] \*        case? IF  postpone \*    false  EXIT THEN  \ needed to be able to e.g. comment out [THEN]
   ['] <eof> = abort" [THEN] missing"
   \ all oter xt's are just ignored
   false
;
:noname  ( flag --  ) ?EXIT BEGIN  forth-word [if]-decode UNTIL ; IS [IF]

:noname  ( -- )             BEGIN  forth-word [if]-decode UNTIL ; IS [ELSE]

:noname  ( -- )       level-1 ;                                   IS [THEN]

\ ----------------------------------------------------------------------
\ dummies for RS232 interface
\ ----------------------------------------------------------------------

: term-emit  drop ;
: term-flush ;
: term-key  $40 ;
: term-key? false ;
: close-port ;

\ ----------------------------------------------------------------------
\ File I/O
\ ----------------------------------------------------------------------
Forth definitions

: fopen        ( <name> mode -- fid )
   BL word dup c@ 0= abort" source file name required"
   count rot open-file abort" file not found"
;
: fclose       ( fid -- )
   close-file abort" file close failed"
;
: fcreate      ( <name> mode -- fid )
   BL word dup c@ 0= abort" file name required"
   count rot create-file abort" file create error"
;
: fsize        ( fid -- ud )
   file-size abort" file size error"
;
: fposition    ( fid -- ud )
   file-position abort" file position error"
;
: freposition  ( ud fid -- )
   reposition-file abort" file reposition error"
;
: fwrite       ( addr len fid -- )
   write-file abort" file write error"
;
: fwrite-line  ( addr len fid -- )
   write-line abort" fwrite-line error"
;
: fread        ( addr len fid -- len' )
   read-file abort" fread error"
;
: fread-line   ( addr len fid -- len' f ) \ f = 0 when end of file reached
   read-line abort" fread-line error"
;

include vhdl.fs  \ simple VHDL interpreter for architecture_pkg
