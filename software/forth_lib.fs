\ ----------------------------------------------------------------------
\ @file : forth.fs
\ ----------------------------------------------------------------------
\
\ Last change: KS 02.04.2022 19:29:11
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
\ @brief : Standard Forth words composed of microCore instructions.
\
\ Version Author   Date       Changes
\   210     ks   14-Jun-2020  initial version
\ ----------------------------------------------------------------------
Target

~ : do         ( limit start -- count end )  under - 1- swap 1- ;
  Host: DO     ( n1 n2 -- )  ( R: -- n3 n4 ) ?comp T  do >r FOR H ;

~ : ?do        ( limit start -- count end )  under -    swap 1- ;
  Host: ?DO    ( n1 n2 -- )  ( R: -- n3 n4 ) ?comp T ?do >r ?FOR H ;

~ : loop       ( -- ) ; noexit \ dummy definition
  Host: LOOP   ( -- )  ( R: n1 n2 -- n1 n2 )  ( R: n1 0 -- ) ?comp T NEXT rdrop H ;

~ Macro: bounds ( start len -- limit start )  ?comp T over >r + r> H ;

~ EXTENDED [NOTIF]

   ~ : I       ( -- i )           r> r> r@ over >r swap - swap BRANCH ; noexit
   ~ : flag?   ( mask -- f )      Flag-reg @ and ;

~ [THEN]

~ : ctrl?      ( mask -- f )      Ctrl-reg @ and ;

~ : within  ( w [low [high -- flag )   over - >r - r> ; noexit       \ fall into u<
  : u<      ( n1 n2 -- f )             - drop carry? 0= ;
~ : u>      ( n1 n2 -- f )        swap - drop carry? 0= ;
~ : case?   ( n1 n2 -- n1 ff | tf ) \ Selection Operator.
     over - IF  false  EXIT THEN  drop true
  ;
~ : max     ( n1 n2 -- max )      2dup < IF  nip EXIT THEN  drop ;
~ : min     ( n1 n2 -- min )      2dup < IF  drop EXIT THEN  nip ;
~ : umin    ( n1 n2 -- max )      2dup - drop carry? IF  nip   EXIT THEN  drop ;
~ : umax    ( n1 n2 -- min )      2dup - drop carry? IF  drop  EXIT THEN  nip ;
~ : abs     ( n -- u )            dup 0< IF  negate  THEN ;

~ EXTENDED [NOTIF]
   ~ : +sat ( n1 n2 -- n3 )       + ovfl? IF  0< #signbit xor  THEN ;
~ [THEN]
~ : u+sat      ( u n -- u' )      swap #signbit tuck xor rot +sat xor ;

~ : 2@      ( addr -- d )         ld cell+ @ swap ;
  Host: 2@  ( addr -- d )         comp? dbg? or IF T 2@ H EXIT THEN  dup cell+ d@  swap d@ dtarget ;
~ : 2!      ( d addr -- )         st cell+ ! ;
  Host: 2!  ( d addr -- )         comp? dbg? or IF T 2! H EXIT THEN  >r d>target r@ d!  r> cell+ d! ;
~ : m+      ( d n -- d )          extend ; noexit                    \ fall into d+
  : d+      ( d1 d2 -- d3 )       >r rot + swap r> +c ;
~ : d-      ( d1 d2 -- d3 )       >r rot swap- swap r> not +c ;
~ : dabs    ( d -- +d )           dup 0< 0= ?EXIT ; noexit           \ fall into dnegate
  : dnegate ( d1 -- d2 )          swap negate swap not 0 +c ;
~ : d=      ( d1 d2 -- f )        d- d0= ;

~ : pack    ( c u -- u' )         8 shift swap $FF and or ;
~ : unpack  ( u -- c u' )         dup $FF and swap -8 shift ;

~ : 2nip    ( d1 d2 -- d2 )           rot drop rot drop ;
~ : 2swap   ( d1 d2 -- d2 d1 )        rot >r rot r> ;
~ : 2over   ( d1 d2 -- d1 d2 d1 )     >r >r 2dup r> -rot r> -rot ;
~ : 2rot    ( d1 d2 d3 -- d3 d1 d2 )  2swap 2>r 2swap 2r> ;
~ : 2-rot   ( d1 d2 d3 -- d2 d3 d1 )  2>r 2swap 2r> 2swap ;

~ WITH_MULT [IF]

   ~ : d2*     ( d1 -- d2 )           1 ; noexit                        \ fall into dshift
     : dshift  ( ud n -- ud' )  \ Limitation: |n| <= data_width
        dup 0< IF  >r r@ mshift rot r> mshift drop or swap  EXIT THEN   \ shift right
        >r   swap r@ mshift   rot r> mshift drop or                     \ shift left
     ;
   ~ : ud2/    ( ud1 -- ud2 )        -1 dshift ;

   ~ : d2/     ( d1 -- d2 )          -1 ; noexit                        \ fall into dashift
     : dashift ( d n -- d' )
        dup 0< IF  >r r@ mashift rot r> mshift drop or swap  EXIT THEN  \ shift right
        >r   swap r@ mshift   rot r> mshift drop or                     \ shift left
     ;

~ [ELSE]

   ~ : d2*     ( ud -- ud' )   swap 2* swap c2* ;
   ~ : ud2/    ( ud -- ud' )   u2/ swap c2/ swap ;
   ~ : dshift  ( ud n -- ud' )
        dup 0< IF  not FOR  u2/ swap c2/ swap  NEXT  EXIT THEN       \ shift right
                      ?FOR  swap 2* swap c2*   NEXT                  \ shift left
     ;
   ~ : d2/     ( d -- d' )     2/ swap c2/ swap ;
   ~ : dashift ( d n -- d' )
        dup 0< IF  not FOR  2/ swap c2/ swap  NEXT  EXIT THEN        \ shift right
                      ?FOR  swap 2* swap c2*   NEXT                  \ shift left
     ;
   ~ : um*     ( u1 u2 -- udprod )   umultiply ;
   ~ : m*      ( n1 n2 -- dprod )    2dup xor >r abs swap abs um* r> 0< IF  dnegate  THEN ;
   ~ : *       ( n1 n2 -- prod )     um* multl ;
     Host: *   ( n1 n2 -- n3 )       comp? dbg? or IF T * H EXIT THEN  * ;

   ~ WITH_FLOAT [IF]
   
      ~ : round   ( dm -- m' )
          over 0< 0= IF  nip  EXIT THEN   \ < 0.5
          swap 2*    IF  1+   EXIT THEN   \ > 0.5
          dup 1 and +                     \ = 0.5, round to even
        ;
      ~ : *.      ( n1 x -- n2 )        over abs um* round swap 0< IF  negate  THEN ;
   
   ~ [THEN]
~ [THEN]

~ : ud*     ( ud u  -- udprod )       tuck um* drop >r um* r> + ;
~ : u/mod   ( u1 u2 -- urem uquot )   0 swap ; noexit                \ fall into um/mod
  : um/mod  ( ud u  -- urem uquot )   udivide ;
~ : u/      ( u1 u2 -- uquot )        u/mod nip ;
~ : umod    ( u1 u2 -- urem )         u/mod drop ;
~ : ud/mod  ( ud u  -- urem udquot )  tuck u/mod >r swap um/mod r> ;

~ EXTENDED [IF]

   ~ : /mod    ( n1 n2 -- rem quot )     swap extend rot ; noexit    \ fall into m/mod
     : m/mod   ( d n -- rem quot )       sdivide ;
   ~ : sqrt     ( u -- urem uroot )      uroot ;

~ [ELSE]

   ~ : +! ( n addr -- )       Status @ -rot di  ld -rot   + swap !   Status ! ;

   ~ : /mod    ( n1 n2 -- rem quot )     swap extend rot ; noexit    \ fall into m/mod
     : m/mod   ( d n -- rem quot )
        dup >r   abs >r   dup 0< IF  r@ +  THEN  r> um/mod
        r@ 0< IF  negate over IF  swap r@ + swap 1-  THEN THEN  rdrop
     ;
   ~ : sqrt    ( u -- urem uroot )
        0 tuck   [ data_width 2/ 1- ] Literal
        FOR  2 dshift swap >r   swap 2 shift 1+
                 2dup - 0< 0= IF  tuck - swap 2 +  THEN
                 u2/ swap r> swap
        NEXT  nip swap
     ;
~ [THEN]

~ : /       ( n1 n2 -- quot )         /mod nip ;
~ : mod     ( n1 n2 -- urem )         /mod drop ;

~ : */mod   ( n1 n2 n3 -- n4 n5 )     -rot m*   rot m/mod ;
~ : */      ( n1 n2 n3 -- n4 )        */mod nip ;

~ : 2**     ( n -- 2**n )             1 swap shift ;
  Host: 2**   ( u1 -- u2 )            comp? dbg? or IF T 2** H EXIT THEN  2** ;

~ WITH_MULT [IF]

   : log2   ( u -- u' )               ulog ;

~ [ELSE]

   : log2   ( frac -- log2 )   \ Bit-wise Logarithm (K.Schleisiek/U.Lange)
      0   data_width
      ?FOR  2* >r   dup um*
         dup 0< IF  r> 1+ >r  ELSE  d2*  THEN     \ correction of 'B(i)' and 'A(i)'
         round   r>                               \ A(i+1):=A(i)*2^(B(i)-1)
      NEXT  nip
   ;
~ [THEN]

~ : inc      ( addr -- )         1 swap +! ;
~ : dec      ( addr -- )        -1 swap +! ;
~ : on       ( addr -- )        -1 swap ! ;
~ : off      ( addr -- )         0 swap ! ;
~ : erase    ( addr len -- )     0 ; noexit  \ fall into fill
  : fill     ( addr len u -- )   -rot ?FOR  under st cell+  NEXT  2drop ;
~ : move     ( addr_from addr_to ucount -- )
     >r 2dup u< IF ( cmove> )
        r@ + swap r@ +  r> ?FOR  cell- ld -rot  swap cell- st  swap  NEXT
     ELSE    ( cmove )
         1 - swap  1 -  r> ?FOR  cell+ ld -rot  swap cell+ st  swap  NEXT
     THEN  2drop
  ;
~ : place    ( addr len to -- )  over swap st cell+ swap move ;

~ : ,        ( n -- )  here ! 1 allot ;
  Host: ,    ( n -- )  comp? dbg? or IF T , H EXIT THEN Tdp @ d!   1 Tdp +! ;

~ : sleep    ( n -- )           ahead | ; noexit   \ fall into continue
  : continue ( time -- )        BEGIN  dup time? UNTIL drop ;
~ : elapsed  ( time -- ticks )  time swap - ;
~ : ms ; noexit   Host: ms ( msec -- ticks )
     ticks_per_ms
     comp? IF  lit, T * H EXIT THEN
     dbg? IF  t> * >t  EXIT THEN
     *
  ; immediate
~ : sec ; noexit   Host: sec  ( sec -- ticks )
     [ ticks_per_ms &1000 * ] Literal
     comp? IF  lit, T * H EXIT THEN
     dbg? IF  t> * >t  EXIT THEN
     *
  ; immediate
\ ----------------------------------------------------------------------
\ Catch and throw, no multitasking
\ ----------------------------------------------------------------------

~ Variable Rcatch   0 Rcatch !

  : rstack>  ( -- rsp )   Rsp @ ;    \ A call in order to push TOR into the return stack memory

  : catch ( xt -- error# | 0 )       \ Return address is already on rstack
     Dsp @ >r               ( xt )   \ Save data stack pointer, xt fills NOS slot
     Rcatch @ >r            ( xt )   \ Save previous Rcatch
     rstack> Rcatch !       ( xt )   \ Fill TOR and set Rcatch to RSP
     execute                (  )     \ Execute the word passed on the stack
     r> Rcatch !            (  )     \ Restore previous Rcatch
     rdrop                  (  )     \ Discard saved stack pointer
     0                      ( 0 )    \ Signal normal completion
  ;
  : throw ( error# -- )       \ Returns to context saved by CATCH
     ?dup 0= ?EXIT                   \ Don't throw 0
     Rcatch @ ?dup 0= IF #catch-not-initialized message THEN
     >rstack           ( err# )      \ Return to saved return stack context
     r> Rcatch !       ( err# )      \ Restore previous #t-catch
     r> swap >r        ( saved-dsp ) \ save err# temporarily on rstack
     >dstack r>        ( err# )      \ Change stack pointer
  ; \ EXIT will return to the caller of CATCH, because the return stack has
    \ been restored to the state that existed when CATCH was executed.
