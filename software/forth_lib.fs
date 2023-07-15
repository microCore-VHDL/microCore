\ ----------------------------------------------------------------------
\ @file : forth.fs
\ ----------------------------------------------------------------------
\
\ Last change: KS 12.07.2023 22:14:40
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
\  2400     ks   03-Nov-2022  byte addressing using byte_addr_width
\ ----------------------------------------------------------------------
Target

~ : do         ( limit start -- count end )  under - 1- swap 1-  r> swap >r BRANCH ;
  Host: DO     ( n1 n2 -- )  ( R: -- n3 n4 ) ?comp T  do  FOR H ;

~ : ?do        ( limit start -- count end )  under -    swap 1-  r> swap >r BRANCH ;
  Host: ?DO    ( n1 n2 -- )  ( R: -- n3 n4 ) ?comp T ?do ?FOR H ;

~ : loop       ( -- ) ; noexit \ dummy definition
  Host: LOOP   ( -- )  ( R: n1 n2 -- n1 n2 )  ( R: n1 0 -- ) ?comp T NEXT rdrop H ;

~ with_PLOOP [IF]
   ~ : +loop ; noexit Host: +LOOP ( n -- ) ( R: n1 n2 -- n1 n2 )  ( R: n1 0 -- )
        ?comp T (+loop NEXT rdrop H ;
~ [ELSE]
   ~ : (+loop  ( n -- ) ( R: i -- i' )
        1- dup 0< swap  r> r> rot - rot carry? xor and >r BRANCH ; noexit
     : +loop ; noexit Host: +LOOP ( n -- ) ( R: n1 n2 -- n1 n2 )  ( R: n1 0 -- )
        ?comp T (+loop NEXT rdrop H ;
~ [THEN]

~ : bounds     ( start len -- limit start )   over swap + swap ;

~ with_INDEX [NOTIF] ~ : I       ( -- i )           r> r> r@ over >r swap - swap BRANCH ; ~ [THEN]

~ with_FLAGQ [NOTIF]
   ~ : flag?   ( mask -- f )      Flags @ and ;
~ [THEN]

~ with_PLUSST [NOTIF]
   ~ : +!      ( n addr -- )      swap over [di @ + swap ! di] ; \ interrupt save
~ [THEN]

~ : ctrl?      ( mask -- f )      Ctrl-reg @ and ;

~ : u<      ( n1 n2 -- f )        - drop carry? 0= ;
  Host: u<                        comp? dbg? or IF T u< H EXIT THEN u< ;
~ : u>      ( n1 n2 -- f )        swap u< ;
  Host: u>                        comp? dbg? or IF T u> H EXIT THEN u> ;
~ : within  ( w [low [high -- flag )   over - -rot - swap u< ;
~ : case?   ( n1 n2 -- n1 ff | tf ) \ Selection Operator.
     over - IF  false  EXIT THEN  drop true
  ;
~ : max     ( n1 n2 -- max )      2dup < IF  nip  EXIT THEN  drop ;
~ : min     ( n1 n2 -- min )      2dup < IF  drop EXIT THEN  nip ;
~ : umin    ( n1 n2 -- max )      2dup - drop carry? IF  nip   EXIT THEN  drop ;
~ : umax    ( n1 n2 -- min )      2dup - drop carry? IF  drop  EXIT THEN  nip ;
~ : abs     ( n -- u )            dup 0< IF  negate  THEN ;

~ with_ADDSAT [NOTIF]
   ~ : +sat ( n1 n2 -- n3 )       + ovfl? IF  0< #signbit xor  THEN ;
~ [THEN]
~ : u+sat   ( u n -- u' )         swap #signbit tuck xor rot +sat xor ;

~ : 2@      ( addr -- d )         ld cell+ @ swap ;
  Host: 2@  ( addr -- d )         comp? dbg? or IF T 2@ H EXIT THEN  dup tcell+ d@  swap d@ dtarget ;
~ : 2!      ( d addr -- )         st cell+ ! ;
  Host: 2!  ( d addr -- )         comp? dbg? or IF T 2! H EXIT THEN  >r d>target r@ d!  r> tcell+ d! ;
~ : d+      ( d1 d2 -- d3 )       >r rot + swap r> +c ;
~ : m+      ( d n -- d )          extend d+ ;
~ : d-      ( d1 d2 -- d3 )       >r rot swap- swap r> not +c ;
~ : dnegate ( d1 -- d2 )          swap negate swap not 0 +c ;
~ : dabs    ( d -- +d )           dup 0< 0= ?EXIT dnegate ;
~ : d=      ( d1 d2 -- f )        d- d0= ;

~ : pack    ( c u -- u' )         8 shift swap $FF and or ;
~ : unpack  ( u -- c u' )         dup $FF and swap -8 shift ;

~ : 2nip    ( d1 d2 -- d2 )           rot drop rot drop ;
~ : 2swap   ( d1 d2 -- d2 d1 )        rot >r rot r> ;
~ : 2over   ( d1 d2 -- d1 d2 d1 )     >r >r 2dup r> -rot r> -rot ;
~ : 2rot    ( d1 d2 d3 -- d3 d1 d2 )  2swap 2>r 2swap 2r> ;
~ : 2-rot   ( d1 d2 d3 -- d2 d3 d1 )  2>r 2swap 2r> 2swap ;

~ WITH_MULT [IF]

   ~ : dshift  ( ud n -- ud' )  \ Limitation: |n| <= data_width
        dup 0< IF  >r r@ mshift rot r> mshift drop or swap  EXIT THEN   \ shift right
        >r   swap r@ mshift   rot r> mshift drop or                     \ shift left
     ;
   ~ : d2*     ( d1 -- d2 )           1 dshift ;
   ~ : ud2/    ( ud1 -- ud2 )        -1 dshift ;

   ~ : dashift ( d n -- d' )
        dup 0< IF  >r r@ mashift rot r> mshift drop or swap  EXIT THEN  \ shift right
        >r   swap r@ mshift   rot r> mshift drop or                     \ shift left
     ;
   ~ : d2/     ( d1 -- d2 )          -1 dashift ;

~ [ELSE] \ without_MULT

   ~ : d2*     ( ud -- ud' )   swap 2* swap c2* ;
   ~ : ud2/    ( ud -- ud' )   u2/ swap c2/ swap ;
   ~ : dshift  ( ud n -- ud' )
        dup 0< IF  not FOR  ud2/  NEXT  EXIT THEN  \ shift right
                      ?FOR  d2*   NEXT             \ shift left
     ;
   ~ : d2/     ( d -- d' )     2/ swap c2/ swap ;
   ~ : dashift ( d n -- d' )
        dup 0< IF  not FOR  d2/  NEXT  EXIT THEN   \ shift right
                      ?FOR  d2*  NEXT              \ shift left
     ;
   ~ : um*     ( u1 u2 -- udprod )   umultiply ;
   ~ : m*      ( n1 n2 -- dprod )    2dup xor >r abs swap abs um* r> 0< IF  dnegate  THEN ;
   ~ : *       ( n1 n2 -- prod )     um* multl ;
     Host: *   ( n1 n2 -- n3 )       comp? dbg? or IF T * H EXIT THEN  * ;

~ [THEN] \ WITH_MULT

~ : ud*     ( ud u  -- udprod )       tuck um* drop >r um* r> + ;
~ : um/mod  ( ud u  -- urem uquot )   udivide ;
~ : u/mod   ( u1 u2 -- urem uquot )   0 swap um/mod ;

~ : u/      ( u1 u2 -- uquot )        u/mod nip ;
~ : umod    ( u1 u2 -- urem )         u/mod drop ;
~ : ud/mod  ( ud u  -- urem udquot )  tuck u/mod >r swap um/mod r> ;

~ with_SDIV [IF]
   ~ : m/mod   ( d n -- rem quot )       sdivide ;
~ [ELSE]
   ~ : m/mod   ( d n -- rem quot )
        dup >r 0< IF  dnegate  r@ negate  ELSE  r@  THEN
        over   0< IF  tuck + swap um/mod   ovfl? over 0< not
                  ELSE            um/mod   ovfl? over 0<
                  THEN  or -rot  \ the ovfl-bit
        r> 0< IF  swap negate swap  THEN
        rot IF  #ovfl st-set  ExIT THEN  #ovfl st-reset
     ;
~ [THEN]

~ with_SQRT [IF]
   ~ : sqrt     ( u -- urem uroot )      uroot ;
~ [ELSE]
   ~ ODD_DATA_WIDTH [IF]  \ odd data width

      ~ : sqrt    ( u -- urem uroot )
           0 tuck   1 dshift swap >r   swap 2* 1+
           2dup - 0< 0= IF  tuck - swap 1+  THEN
           u2/ swap r> swap
           [ data_width 2/ 1- ] Literal
           FOR  2 dshift swap >r   swap 2 shift 1+
                2dup - 0< 0= IF  tuck - swap 2 +  THEN
                u2/ swap r> swap
           NEXT  nip swap
        ;

   ~ [ELSE]   \ even data width

      ~ : sqrt    ( u -- urem uroot )
           0 tuck   [ data_width 2/ 1- ] Literal
           FOR  2 dshift swap >r   swap 2 shift 1+
                2dup - 0< 0= IF  tuck - swap 2 +  THEN
                u2/ swap r> swap
           NEXT  nip swap
        ;

   ~ [THEN]
~ [THEN]

with_FMULT WITH_FLOAT or WITH_MULT 0= and [IF]
   ~ : round   ( dm -- m' )
       over 0< 0= IF  nip  EXIT THEN   \ < 0.5
       swap 2*    IF  1+   EXIT THEN   \ > 0.5
       dup 1 and +                     \ = 0.5, round to even
     ;
   ~ : *.      ( n1 x -- n2 )        over abs um* round swap 0< IF  negate  THEN ;
[THEN]

~ : +2!     ( n addr -- )          Status @ -rot di   tuck 2@ rot extend d+ rot 2!  Status ! ;

~ : /mod    ( n1 n2 -- rem quot )  swap extend rot m/mod ;
~ : /       ( n1 n2 -- quot )      /mod nip ;
~ : mod     ( n1 n2 -- urem )      /mod drop ;

~ : */mod   ( n1 n2 n3 -- n4 n5 )  -rot m*   rot m/mod ;
~ : */      ( n1 n2 n3 -- n4 )     */mod nip ;

~ : 2**     ( n -- 2**n )          1 swap shift ;
  Host: 2**   ( u1 -- u2 )         comp? dbg? or IF T 2** H EXIT THEN  2** ;

~ with_LOGS WITH_FLOAT or WITH_MULT and [IF]
   ~ : log2 ( u -- u' )            ulog ;
~ [ELSE]
   ~ : log2   ( frac -- log2 )   \ Bit-wise Logarithm (K.Schleisiek/U.Lange)
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

~ WITH_BYTES [IF]  \ byte addressed

  ~ : fill     ( caddr len u -- )  -rot ?FOR  under cst 1+ NEXT  2drop ;
  ~ : erase    ( addr len -- )     0 fill ;
  ~ : move     ( caddr_from caddr_to ucount -- )
       >r 2dup u< IF ( cmove> )
          r@ + swap r@ +  r> ?FOR  1- cld -rot  swap 1- cst  swap  NEXT
       ELSE    ( cmove )
           1 - swap  1 -  r> ?FOR  1+ cld -rot  swap 1+ cst  swap  NEXT
       THEN  2drop
    ;
  ~ : place    ( addr len to -- )  over swap cst 1+ swap move ;
  ~ : c,       ( char -- )         here c!   1 allot ;
    Host: c,   ( char -- )         comp? dbg? or IF  T c, H  EXIT THEN  Tdp @ cd!   1 Tdp +! ;

~ [ELSE]  \ cell addressed

  ~ : fill     ( addr len u -- )   -rot ?FOR  under st 1+  NEXT  2drop ;
  ~ : erase    ( addr len -- )     0 fill ;
  ~ : move     ( addr_from addr_to ucount -- )
       >r 2dup u< IF ( cmove> )
          r@ + swap r@ +  r> ?FOR  1- ld -rot  swap 1- st  swap  NEXT
       ELSE    ( cmove )
           1 - swap  1 -  r> ?FOR  1+ ld -rot  swap 1+ st  swap  NEXT
       THEN  2drop
    ;
  ~ : place    ( addr len to -- )  over swap st 1+ swap move ;

~ [THEN]

~ : continue ( time -- )        BEGIN  dup time? UNTIL drop ;
~ : sleep    ( n -- )           ahead continue ;
~ : elapsed  ( time -- ticks )  time swap - ;
~ : ms ; noexit   Host: ms ( msec -- ticks )
     ticks_per_ms
     comp? IF  lit, T * H EXIT THEN
     dbg?  IF  t> * >t    EXIT THEN
     *
  ; immediate
~ : sec ; noexit   Host: sec  ( sec -- ticks )
     [ ticks_per_ms &1000 * ] Literal
     comp? IF  lit, T * H EXIT THEN
     dbg?  IF  t> * >t    EXIT THEN
     *
  ; immediate
\ ----------------------------------------------------------------------
\ Catch and throw, when multitask.fs not loaded
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
