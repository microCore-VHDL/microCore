\ ----------------------------------------------------------------------
\ @file : objects.fs
\ ----------------------------------------------------------------------
\
\ Last change: KS 27.02.2021 19:26:45
\ Project : microCore
\ Language : gforth_0.6.2
\ Last check in : $Rev: 619 $ $Date:: 2021-01-20 #$
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
\ @brief : OOP package. Its primary use is for operator overloading.
\ - no polymorphism nor vtables,
\ - only early binding at compile time.
\
\ Version Author   Date       Changes
\   2300    ks   19-Feb-2021  Initial release
\ ----------------------------------------------------------------------
Forth definitions

Variable Last-class     Last-class off
Variable 'classroot    'classroot off

Variable (doClass  : doClass ( -- xt )  (doClass @ ;
Variable (doObj    : doObj   ( -- xt )  (doObj @ ;

: set-class   ( class -- )  dup Last-class !  Class-context ! ;

: class-def?  ( -- f )      Current @ cell- @ doClass = ;

: ?class-def  ( -- )
   class-def? Class-context @ or 0= ABORT" No Class context defined"
;
: Class       ( <name> -- )
   Vocabulary  0 , 0 , 'classroot @ ,
\ | search-xt | wordlist | Voclink | ext || attr | size | parent |
DOES>  ( -- )  [ here (doClass ! ] set-class ;

: c.last      ( cpa -- addr )  @ cell+ ;   \ cpa = Code Pointer Address

: c.attr      ( cpa -- addr )  @ [ 4 cells ] Literal + ;

: c.size      ( cpa -- addr )  @ [ 5 cells ] Literal + ;

: c.parent    ( cpa -- addr )  @ [ 6 cells ] Literal + ;

1 Constant #sealed  \ attr bit

: ?unsealed   ( -- )
   Current c.attr @ #sealed and ABORT" Class already sealed"
;
:noname  ( addr len -- xt | 0 )
   Last-class >r  BEGIN  r@ @
   WHILE  2dup r@ @ find-name-in
          ?dup IF  nip nip rdrop  EXIT THEN
          r> c.parent >r
   REPEAT
   rdrop 2drop false
; IS find-methods

: >object  ( addr -- obj )  dup @ swap cell+ cell+ @ set-class ;

: (object   ( addr <name> -- )
   Method   dup @ Constant
   Last-class c.size @ swap +!
   here   Variables @ , Variables !
   Last-class @ ,
\ | Tdp | varlink | class |
DOES> ( -- obj )  [ here (doObj ! ]   >object
   comp? IF  lit,  EXIT THEN
   dbg?  IF  >t         THEN
;
\ -------------------------------------------------------------------------
\ Root definitions
\ -------------------------------------------------------------------------
Root definitions Forth

: Self     ( -- )
   class-def? IF  Current  ELSE  Last-class  THEN  @ set-class
; immediate

: .classes  ( -- )  space   Voclink
   BEGIN  @ ?dup
   WHILE  dup cell- cell-
          dup cell- @ doClass =
          IF  ?cr dup .wordname  THEN  drop
   REPEAT
;
: Object ( -- )   Tdp (object ;

\ -------------------------------------------------------------------------
\ ClassRoot definitions
\ -------------------------------------------------------------------------
Forth definitions

: classorder  ( -- )  \ put class into normal search order until closing ;
   Method   r> Context save >r   Last-class @ Context !
;
Class ClassRoot
' ClassRoot >body 'classroot ! \ every class inherits ClassRoot
' ClassRoot >body Current !    \ ClassRoot definitions

: ..      ( -- )           Method ; immediate

: addr    ( obj -- addr )  Method ; immediate

: definitions  ( -- )      Method Last-class @ Current ! ;

: '       ( <name> -- xt ) ?exec classorder T ' H ;
          
: h'      ( <name> -- xt ) ?exec classorder ' ;
          
: show    ( <name> -- )    ?exec classorder show ;
                          
: see     ( <name> -- )    ?exec classorder see  ;
                          
: allot   ( u -- )         ?exec Method ?class-def Current c.size +! ;
                          
: seal    ( -- )           ?exec Method ?class-def Current c.attr dup @ #sealed or swap ! ;
                          
: size    ( -- u )
   Method Last-class c.size @
   comp? IF  postpone Literal  EXIT THEN
   dbg?  IF  >t                     THEN
; immediate

: units   ( u1 -- u2 )     ?exec Method Last-class c.size @ * ;

: inherit ( -- )           ?exec Method ?class-def
   Current c.last @   Current c.size @ or ABORT" inherit only once"
   Last-class c.size @ Current c.size !
   Last-class        @ Current c.parent !
;
: words   ( -- )           ?exec Method   Context @   Last-class @
   BEGIN  Context ! words  Context c.parent @ ?dup 0= UNTIL  Context !
;
: order   ( -- )           ?exec Method Last-class >r
   BEGIN  r@ @ ?dup WHILE  .voc  r> c.parent >r  REPEAT  rdrop
   ."    "  H Current @ .voc
;
: Constant ( <name> n -- )
   Method   dbg? IF t> THEN  Constant
   here  Constants @ , Constants !
   Last-class @ ,
\ | n | varlink | class |
DOES> ( -- obj )   >object
   comp? IF  lit,  EXIT THEN
   dbg?  IF  >t         THEN
;
: Object  ( <name> -- )   Method Object ;

: Attribute ( <name> -- )
   ?exec Method ?class-def ?unsealed
   Tcreate Last-class @ ,
   Current c.size @   here 3 cells - !
   Last-class c.size @   Current c.size +!
\ | offset | Varlink | class |
DOES>  ( object1 -- object2 )   >object
   comp? IF  ?dup 0= ?EXIT  lit, T + H  EXIT THEN
   dbg?  IF  t> + >t                    EXIT THEN
   +
;

' [ Alias [ immediate  \ for debugging
' ] Alias ]            \ for debugging

\ -------------------------------------------------------------------------
\ Target definitions
\ -------------------------------------------------------------------------
Target definitions Forth

' Class     Alias Class
' ClassRoot Alias ClassRoot
' Method    Alias Method

Forth definitions

\ ****************************************************************
\ Examples for basic Objects: Cell and Point
\ These can not be compiled here, because first the cross-compiler
\ must have been completely loaded.
\ ****************************************************************
\ Target
\
\ Class Cell   Cell definitions
\ 1 Cell allot   Cell seal
\ Macro: @    ( obj -- n )   T @ H ;
\ Macro: !    ( n obj -- )   T ! H ;
\      : +!   ( n obj -- )   +! ;
\      : on   ( obj -- )     on ;
\      : off  ( obj -- )     off ;
\      : ?    ( obj -- )     @ . ;
\ Target
\ 
\ Class Point  Point definitions
\    Cell Attribute X
\    Cell Attribute Y
\ Point seal
\ : set   ( X Y obj -- )     swap over Self Y !   Self X ! ;
\ : ?     ( obj -- )         dup Point X ?   Point Y ? ;
\ Target
\
\ Point Object Punkt   init: init-Punkt ( -- )   1 2 Punkt set ;
\
\ ****************************************************************
\ int16 with sign extension
\ ****************************************************************
\ 
\ Class int16   int16 definitions
\ 1 int16 allot   int16 seal
\      : @  ( addr -- n )  @ $FFFF and dup $8000 and IF  $FFFF not or  THEN ;
\ Macro: !  ( n addr -- )  T ! H ;
\ Target
\
\      int16 Object Dies     \ an int16 variable in the data memory
\ $200 int16 Constant Das    \ an int16 variable at address $200
