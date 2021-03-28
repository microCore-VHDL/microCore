\ ----------------------------------------------------------------------
\ @file : messages.fs
\ ----------------------------------------------------------------------
\
\ Last change: KS 24.03.2021 17:52:17
\ Last check in: $Rev: 674 $ $Date:: 2021-03-24 #$
\ @project: microCore
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
\ @brief : Definition of warnings, errors, and execution tokens on the host.
\
\ Version Author   Date       Changes
\   210     ks   14-Jun-2020  initial version
\ ----------------------------------------------------------------------
Forth definitions

\ ----------------------------------------------------------------------
\ Message# defines target signals for target/host interaction.
\ ----------------------------------------------------------------------

: Message#     ( n -- )  #datamask and
   source> Target definitions over T Constant H
   >source Forth  definitions Constant
;
\ warnings
   $4000 Message# #string-overflow

\ errors
#signbit Message# #breakpoint
      -1 Message# #warmboot
      -2 Message# #all-tasks-busy
      -3 Message# #task-not-linked
      -4 Message# #catch-not-initialized
      -5 Message# #not-my-semaphore

: .error  ( -n -- )
   #warmboot                    case? IF  ." warmboot "                     EXIT THEN
   #all-tasks-busy              case? IF  ." all-tasks-busy "               EXIT THEN
   #task-not-linked             case? IF  ." task-not-linked "              EXIT THEN
   #catch-not-initialized       case? IF  ." catch-not-initialized "        EXIT THEN
   #not-my-semaphore            case? IF  ." not-my-semaphore "             EXIT THEN
   ." error: " signextend .
;
: .warning  ( +n -- )  cr ." warning: "
   #string-overflow case? IF  ." string-overflow " EXIT THEN
   .hex
;
\ ----------------------------------------------------------------------
\ target words, which can be executed during compilation
\ ----------------------------------------------------------------------
SIMULATION [IF] Target definitions Forth

   : .     ( n -- )         ?exec  . ;
   : .r    ( n u -- )       ?exec .r ;
   : u.    ( u -- )         ?exec tu. ;
   : d.    ( d -- )         ?exec d. ;
   : ud.   ( ud -- )        ?exec ud. ;
   : cr    ( -- )           ?exec cr ;
   : here  ( -- addr )      ?exec Tdp @ ;
   : allot ( n -- )         ?exec Tdp +! ;
   : emit  ( char -- )      ?exec emit ;

Forth definitions

   : do-messages ( n -- )   drop ;

[ELSE]
\ ----------------------------------------------------------------------
\ debug words that can be compiled on the target to be executed by the host
\ The matching target side definitions are at the end of debugger.fs
\ ----------------------------------------------------------------------

   $101 Message# #dot
   $102 Message# #dotr
   $103 Message# #udot
   $104 Message# #ddot
   $105 Message# #uddot
   $106 Message# #cret
   $107 Message# #here
   $108 Message# #allot
   $109 Message# #emit

   : do-messages  ( n -- )
      dup #signbit and IF  State off cr .error                       EXIT THEN
      #dot       case? IF  target> signextend .                      EXIT THEN
      #udot      case? IF  target> tu.                               EXIT THEN
      #dotr      case? IF  target> target> signextend swap .r space  EXIT THEN
      #ddot      case? IF  target> target> dtarget d.                EXIT THEN
      #uddot     case? IF  target> target> udtarget ud.              EXIT THEN
      #cret      case? IF  cr                                        EXIT THEN
      #here      case? IF  Tdp @ >target                             EXIT THEN
      #allot     case? IF  target> Tdp +!                            EXIT THEN
      #emit      case? IF  target> emit                              EXIT THEN
      dup $3FFF >      IF  .warning                                  EXIT THEN
      cr ." message: " .hex
   ;

[THEN]
