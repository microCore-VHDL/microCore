\ ----------------------------------------------------------------------
\ @file : monitor.fs
\ ----------------------------------------------------------------------
\
\ Last change: KS 16.03.2022 18:59:36
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
\ @brief : microCore target monitor for the umbilical debugger.
\          The hardware must be configured with an RS232 serial interface.
\
\ Version Author   Date       Changes
\   210     ks   14-Jun-2020  initial version
\ ----------------------------------------------------------------------
Target

Variable tmpStatus

: debugger ( -- )      \ main debugger loop called via break instruction
   Status @ tmpStatus !   #breakpoint >host  r> 1- >host
; noexit  \ fall into monitor

: monitor  ( -- )       BEGIN  0 >host   host> execute  REPEAT ; noexit

: clear    ( ? -- )     Dsp @ [ #ds-depth 1- not ] Literal and Dsp ! ;

: rclear   ( r: ? -- )  r>   Rsp @ [ #rs-depth 1- ] Literal or Rsp !   >r ;

: debug-service     ( -- )  
   rclear  clear   host> drop
   BEGIN  #warmboot >host   host> $5F5 = UNTIL
   BEGIN  $505 >host   host> 0= UNTIL  \ synchronise Host <-> Target communication
   GOTO monitor
; noexit

\ ----------------------------------------------------------------------
\ Some words that are needed by the debugger
\ ----------------------------------------------------------------------

: message      ( n -- )  \ transfer message number to host
   dup >host   0< IF  clear rclear GOTO monitor  THEN
;
: nextstep ( -- ) rdrop  host> >r   tmpStatus @ Status ! ;  \ call and jump (modified via >r)

: depth    ( -- depth )   Dsp @ [ #ds-depth 1- ] Literal and ;

: copyds  ( -- )   depth   dup >host
   dup ?FOR  swap r> swap >r >r         NEXT
       ?FOR  r> r>  swap >r  dup >host  NEXT
;
Variable tmpTOR

: saveTOR     ( -- )          r> r> tmpTOR ! >r ;
: restoreTOR  ( -- )          r> tmpTOR @ >r >r ;

: \>host  ( n -- )            >host ;
: \host>  ( -- n )            host> ;
: \@      ( addr -- n )       @ ;
: \!      ( n addr -- )       ! ;
: \does   ( addr -- addr+1 )  H op_DOES t, T ;
