\ ----------------------------------------------------------------------
\ @file : library.fs
\ ----------------------------------------------------------------------
\
\ Last change: KS 03.08.2022 18:05:44
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
\ @brief : Library loading mechanism. Library words will only be loaded
\ into the target when they are actually used. Even when they are
\ interpreted in the target while debugging.
\
\ Version Author   Date      Changes
\   2200    ks   24-Oct-2020 initial version
\   2300    ks   12-Mar-2021 compiler switch WITH_PROG_RW eliminated
\                            Bugfix in exec-libdef
\ ----------------------------------------------------------------------
Forth definitions

Variable Libgroup   \ points to the following data structure:
                    \ | ptr->libpath | dposition | current | search-order |
Variable Libdepth   \ Checking stack integrity while pre-loading a library

\ ----------------------------------------------------------------------
\ Defining Library words and Predefinitions
\ only Predefined words get searched during lib-loading
\ ----------------------------------------------------------------------

Vocabulary Predefined

Root definitions

' Predefined Alias P immediate

\ ----------------------------------------------------------------------
\ input stream and dictionary manipulations
\ ----------------------------------------------------------------------
Forth definitions

: context>  ( -- addr )
   get-order dup >r   Current @   Targeting @
   'interpreter @   'compiler @   State @   Libload @
   [ ' parser >body ] Literal @   r> 8 + mem-save
;
: >context  ( addr -- )
   mem-restore drop   IS parser   Libload !   State !   'compiler !
   'interpreter !   Targeting !   Current !   set-order
;
: source-position  ( -- udpos )  \ current-sourceposX \ gforth: Loadfile #fill-bytes
   source-id 0= IF  >in @ 0  EXIT THEN
   Loadfile @ file-position throw   #fill-bytes @ >in @ - 0 d-
;
\ group: | ptr->libpath | dposition | current | search-order |

: libgroup,  ( -- )  \ build data structure to remember source code and context
   Libfile @ ,   source-position , ,
   current @ ,   get-order dup 1+ 0 DO , LOOP
;
:noname  ( xt -- )
   >r source-position  2. d-   r> cell+ cell+ @ cell+ 2!
; IS advance-libsource

: libgroup-start  ( -- )
   depth Libdepth @ - abort" stack imbalance during precompilation"
   here Libgroup ! libgroup,
;
: >libgroup   ( group -- )
   dup @ count   2dup R/O open-file throw Loadfile !   Loadfilename 2!
   cell+ dup 2@ Loadfile @ reposition-file throw
   cell+ cell+ dup @ current !
   cell+ dup @ dup >r cells + r> 1+ 0 DO  dup @ swap cell-  LOOP drop   set-order
;
: load-libgroup  ( pfa -- )
   Current @ >r   Libload on   push-file   @ >libgroup
   ['] read-loop catch
   Loadfile @ close-file   pop-file throw   Libload off   r> Current !
                        throw
;
\ Tmarker: | here | there | depth | current | 6 cells save-input stream | \ defined in microcross.fs
:noname  ( -- )
   Tmarker   here over !  cell+ there over !                 
   cell+ depth 1- over !  cell+ Current @ over !   cell+ >r
   Deflength @ negate >in +!  save-input                     \ back up to re-interpret : after restore-target
   Deflength @ >in +!   2 Deflength !
   r> over 1+ 0 DO  tuck ! cell+  LOOP  drop
; IS mark-target

: restore-target  ( i*x -- i*x )
   postpone [   Countfield 2@ !                              \ restore smudged countfield
   Tmarker   dup @ Dp !   cell+ dup @ Tcp !                  \ restore here, there
   cell+ @ >r BEGIN  depth r@ u> WHILE  drop  REPEAT  rdrop  \ restore depth
   Tmarker 3 cells + dup @ Current !
   cell+ dup @ dup cells rot +
   swap 1+ 0 DO  dup @ swap cell- LOOP  drop
   restore-input throw                                       \ restore input stream
   here Init-link @ u< IF  Init-link @ @ Init-link !  THEN   \ restore init: source definition
;
Variable Rsave

: ?libloading  ( -- )
   Tcp @ Tcp-origin @ u< abort" no library loading in TRAP: area"
   Libload @ IF  Verbose @ IF  ." discard "  THEN
       r>   Rsave @ BEGIN  rdepth over - WHILE  rdrop  REPEAT  drop >r \ throw away push-file parameters
       Loadfile @ close-file throw
   EXIT THEN
   comp? IF  Colons @ @ Colons !  THEN
;
Variable Tcp-last  0 Tcp-last !

: exec-libdef  ( pfa -- )
   Verbose @ IF  cr ." libexec   " dup .wordname  THEN
   rdepth 1- Rsave !                                        \ Rsave for ?libloading
   dbg? IF  Tcp-last @ 0= IF  there Tcp-last !  THEN THEN   \ Tcp-last in case of nested library loads
   dup body> >name name>string nip 1+ >in @ min             \ Determine length of token+1
   Tmarker   here over !   cell+   there over !             \ Update here and there in Tmarker
   cell+   depth 3 - over !   cell+ Current @ over !        \ update depth and Current in Tmarker
   cell+ >r >r r@ negate >in +!  save-input   r> >in +!     \ Save input beginning at token
   r> over 1+ 0 DO  tuck ! cell+ LOOP  drop                 \ Update input stream in Tmarker
   dup load-libgroup
   dbg? IF  Tcp-last @ there over - false send-image   0 Tcp-last !  THEN  \ Transfer into the target
   body> >name name>string
   find-name ?dup 0= abort" can not find "
   Verbose @ IF  ." exec "  THEN
   name>int execute                                         \ execute newly loaded word
;
: .loaded  ( -- )  Verbose @ 0= ?EXIT  ." loaded " ;

: .current    Current @ .voc ;

: Libdef  ( <name> -- )
   Create   Libgroup @ ,   0 , ( will link into e.g. colons when actually loaded )
Does> ( -- )  [ here (doLibdef ! ]
   ?libloading   exec? IF  exec-libdef  EXIT THEN
   Verbose @ IF  cr ." libload   " dup .wordname  THEN
   >r restore-target r> rdepth Rsave !   load-libgroup .loaded
;
\ ----------------------------------------------------------------------
\ Library loading
\ ----------------------------------------------------------------------

: pre-compiler ( addr u -- )    [ ' Predefined >body ] Literal search-wordlist IF  execute  THEN ;

: predefining? ( -- f )         [ ' parser >body ] Literal @ ['] pre-compiler = ;

Create ";  ( -- saddr )  ," ;"

: scan-for-;  ( -- )
   BEGIN  BEGIN  name ?dup 0= WHILE  drop refill 0= throw  REPEAT
          "; count   compare 0=
   UNTIL
;
: host-loop ( i*x -- j*x )    BEGIN  interpret refill 0= UNTIL ;

\ ----------------------------------------------------------------------
\ Only Predefinitions are executed during initial library-loading
\ ----------------------------------------------------------------------
Predefined definitions Forth

' order              Alias order
' (                  Alias ( immediate
' \                  Alias \ immediate
' \*                 Alias \* immediate
' \\                 Alias \\
' .(                 Alias .(
' cr                 Alias cr
' .s                 Alias .s
' .                  Alias .
' [IF]               Alias [IF]
' [NOTIF]            Alias [NOTIF]
' [ELSE]             Alias [ELSE]
' [THEN]             Alias [THEN]
' [IFDEF]            Alias [IFDEF]
' [IFUNDEF]          Alias [IFUNDEF]
' true               Alias true
' false              Alias false
' 0=                 Alias 0=
' not                Alias not
' and                Alias and
' or                 Alias or

' SIMULATION         Alias SIMULATION     \ simulating?
' EXTENDED           Alias EXTENDED       \ extended instruction set?
' WITH_MULT          Alias WITH_MULT      \ hardware multiply available?
' WITH_FLOAT         Alias WITH_FLOAT
' WITH_UP_DOWNLOAD   Alias WITH_UP_DOWNLOAD
byte_addr_width 0<>  Constant WITH_BYTES
data_width 1 and 0<> Constant ODD_DATA_WIDTH

: ~   ( -- )   libgroup-start ;

: Version  ( n -- )
   Current @ >r
   source> >r   Predefined definitions dup Constant immediate
   r> >source       Target definitions Constant immediate
   r> Current !
;
: :      ( -- )   Libdef scan-for-; ;

: Host:  ( -- )   scan-for-; ;

P ' : H      Alias Macro:
' Libdef     Alias Create
' Libdef     Alias Variable
' Libdef     Alias Constant
' Libdef     Alias Task
' Libdef     Alias Semaphore

\ ----------------------------------------------------------------------
\ Loading host stuff during precompilation
\ ----------------------------------------------------------------------

: Host  ( i*x -- i*x )
   context> >r   Targeting off   Libload off
   host-compile definitions   ['] host-loop catch
   r> >context   dup 1 = IF  0=  THEN  throw
;
Target definitions Forth

: library  ( <name> -- )    depth Libdepth !
   name dup 0= abort" file name required"
   here   dup Libfile !   over 1+ allot   place    \ store path/file name
   context> >r   ['] pre-compiler IS parser
   Libfile @ count ['] included catch
   r> >context   Libfile off
   dup IF  cr ." libload error " dup .  THEN  throw
;
: ~  ( -- )   \ skip the rest of the input file when compiling from a library
   Libload @ IF  BEGIN  refill 0= UNTIL  THEN
;
Forth definitions
