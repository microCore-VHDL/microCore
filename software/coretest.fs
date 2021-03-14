\ ----------------------------------------------------------------------
\ @file : coretest.fs
\ ----------------------------------------------------------------------
\
\ Last change: KS 06.03.2021 17:50:31
\ Project : microCore
\ Language : gforth_0.6.2
\ Last check in : $Rev: 656 $ $Date:: 2021-03-06 #$
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
\ @brief : microCore test suite for almost all instructions.
\          On simulation it should finish with zero on the stack,
\          when executing it ends with "Message $100" on success
\          and "Message <errno>" on the first error.
\
\ Version Author   Date       Changes
\   210     ks   14-Jun-2020  initial version
\   2300    ks   18-Feb-2021  compiler switch WITH_PROG_RW eliminated
\ ----------------------------------------------------------------------
SIMULATION [IF]  \ simulating

: finis ( errorcode -- )  BEGIN REPEAT ;

[ELSE]  \ Debugging

: finis ( errorcode -- )  Debug-reg ! ['] monitor noop BRANCH ;

[THEN]

Variable Location  1 allot

: zeroEXIT   ( n1 -- n2 )         dup    ?EXIT  0= ;
: zero-EXIT  ( n1 -- n2 )         dup 0= ?EXIT  0= ;

: test-branches  ( -- )
          0  dup 0< IF  not  THEN    IF  $1 finis THEN
         -1  dup 0< IF  not  THEN    IF  $2 finis THEN
    0 u2/ carry? IF  not  THEN       IF  $3 finis THEN
    1 u2/ carry? IF  not  THEN 0=    IF  $4 finis THEN
        -1  zeroEXIT not             IF  $5 finis THEN
         0  zeroEXIT not             IF  $6 finis THEN
        -1 zero-EXIT                 IF  $7 finis THEN
         0 zero-EXIT                 IF  $8 finis THEN
    0 3 FOR 1+ NEXT 4 -              IF  $9 finis THEN
    0 $25 $20 DO  I  LOOP      $24 - IF  $A finis THEN
    + + + +                    $86 - IF  $B finis THEN
                              0 ?dup IF  $C finis THEN
    1 ?dup IF  0=  ELSE  true  THEN  IF  $D finis THEN
;

: cc  ( -- )   Status @ #c xor Status ! ;

: test-unary ( -- )
   -1 0< 0=                            IF  $10 finis THEN
   0 0<                                IF  $11 finis THEN
   -1 2*                     cc carry? IF  $12 finis THEN cc   ( -- -2 )
   not 1 =                          0= IF  $13 finis THEN      ( -- )
   -1 u2/                    cc carry? IF  $14 finis THEN cc   ( $7FF )
   not 2*                    cc carry? IF  $15 finis THEN cc   (    0 )
   0= not                              IF  $16 finis THEN      (    0 )
   $100 1 ashift -2 shift $80 over -   IF  $17 finis THEN      ( -- $80 )
   -7 ashift dup   1 -                 IF  $18 finis THEN      ( -- )
   data_width 1 - shift
   dup #signbit -                      IF  $19 finis THEN      ( -- #signbit )
   -3 ashift
   [ data_width 4 - negate ] Literal
   shift $F - dup                      IF  $1A finis THEN      ( -- )
   0= not  1 0= or                     IF  $1C finis THEN      ( -- )
   #signbit 2/ #signbit dup u2/ or -   IF  $1D finis THEN      ( -- )
   1 2/                      cc carry? IF  $1E finis THEN cc   ( -- 0 )
   0= not                              IF  $1F finis THEN      ( -- )
   0 1+   dup 1 -                      IF  $120 finis THEN     ( -- 1 )
   1-                                  IF  $121 finis THEN     ( -- )
   #signbit -8 shift
   1 data_width 9 - shift -            IF  $122 finis THEN
   #signbit -8 ashift
   data_width 8 - negate shift $FF -   IF  $123 finis THEN
   $A00 5 4 dshift -4 dshift or $A05 - IF  $124 finis THEN
   #signbit dup -8 dashift
   data_width 9 - negate dshift + $200 - IF  $125 finis  THEN
;

#signbit 1 + Constant #8001
#signbit     Constant #8000
#signbit 1 - Constant #7FFF
#signbit 2 - Constant #7FFE

cr .( use load_div_full.fs for multiply / divide tests on a reduced number space. )
cr .( or load_divtest.fs for random tests on the full number space. )

: test-arith  ( -- )
   3 4 +  ( 7 ) 4 -  ( 3 ) 4 2dup +  ( 3 4 7 )
   over swap -  ( 3 4 -3 )
   dup 0< and  ( 3 4 -3 )  negate  ( 3 4 3 )
   or  ( 3 7 ) xor  ( 4 ) 4 and  ( 4 )
   0 -1 0 d+  ( 3 1 )   1- swap 3 -      or  IF  $20 finis THEN
   #7fff  1 +sat #7fff -                     IF  $21 finis THEN
   #7fff -1 +sat #7ffe -                     IF  $22 finis THEN
   #8000 -1 +sat #8000 -                     IF  $23 finis THEN
   #8000  1 +sat #8001 -                     IF  $24 finis THEN
   
      -1     1 um*         >r    -1 - r> or  IF  $25 finis THEN  \ 0000 FFFF
       1    -1 um*         >r    -1 - r> or  IF  $26 finis THEN  \ 0000 FFFF
      -1    -1 um*    -2 - >r     1 - r> or  IF  $27 finis THEN  \ FFFE 0001
      -2    -1 um*    -3 - >r     2 - r> or  IF  $28 finis THEN  \ FFFD 0002
      -1    -2 um*    -3 - >r     2 - r> or  IF  $29 finis THEN  \ FFFD 0002
      -1 #7FFF um* #7FFE - >r #8001 - r> or  IF  $2A finis THEN  \ 7FFE 8001
   #7FFF    -1 um* #7FFE - >r #8001 - r> or  IF  $2B finis THEN  \ 7FFE 8001
       4     5 um* + &20 -                   IF  $2C finis THEN

     &21 0     5 um/mod 4 - >r     1 - r> or IF  $2D finis THEN
      -3 1    -1 um/mod 1 - >r    -2 - r> or IF  $2E finis THEN
      -2 1    -1 um/mod 2 - >r     0 - r> or IF  $2F finis THEN
      -3 1    -2 um/mod 2 - >r     1 - r> or IF  $200 finis THEN
      -1 0 #8000 um/mod 1 - >r #7FFF - r> or IF  $201 finis THEN
       0 1     1 um/mod or
   Status @ #ovfl and #ovfl xor or           IF  $202 finis THEN
   #8000 1 <                              0= IF  $203 finis THEN
   1 #8000 <                                 IF  $204 finis THEN
   1 2     <                              0= IF  $205 finis THEN
   2 1     <                                 IF  $206 finis THEN
;
\ ----------------------------------------------------------------------
\ Floating point
\ ----------------------------------------------------------------------
with_mult with_float and [IF]
   Host

   : round   ( dm -- m' )
      over 0< 0= IF  nip  EXIT THEN   \ < 0.5
      swap 2*    IF  1+   EXIT THEN   \ > 0.5
      dup 1 and +                     \ = 0.5, round to even
   ;
   cell_width data_width - Constant #delta_width \ cell width (host) must be >= data width (target)
   
   : hlog2 ( frac -- log2 )   \ Bit-wise Logarithm (K.Schleisiek/U.Lange)
      #delta_width 0 ?DO  2*  LOOP
      0   data_width 0
      DO  2* >r   dup um*
         dup 0< IF  r> 1+ >r  ELSE  d2*  THEN     \ correction of 'B(i)' and 'A(i)'
         round   r>                               \ A(i+1):=A(i)*2^(B(i)-1)
      LOOP  nip
   ;
   T definitions
   
   #signbit dup u2/ or Constant #C000
   #C000 hlog2         Constant log-C000
   
   #C000    dup u2/ or Constant #E000
   #E000 hlog2         Constant log-E000
   
   Target
   
   Macro: float   ( n -- r )   0 lit, T >float H ;
   &17 data_width - Constant above16
   
   : test-float  ( -- )
      $8001 0 normalize   above16 -        IF  $207 finis THEN
      #signbit u2/ xor   above16 shift 1-  IF  $208 finis THEN
      1 float float> ashift 1-             IF  $209 finis THEN
      -2 float float> ashift 2 +           IF  $20A finis THEN
\ *.
      #signbit   dup $10 *. 8 +            IF  $20B finis THEN
      u2/ $10 *. 4 -                       IF  $20C finis THEN
\ log2
      -1 log2 2 +                          IF  $210 finis THEN
      #signbit log2                        IF  $211 finis THEN
      #C000 log2 log-C000 -                IF  $212 finis THEN
      #E000 log2 log-E000 -                IF  $213 finis THEN
\ sqrt
      -1       dup >r sqrt dup * + r> -    IF  $214 finis THEN
      #signbit dup >r sqrt dup * + r@ -    IF  $215 finis THEN
      r> u2/   dup >r sqrt dup * + r@ -    IF  $216 finis THEN
      r> 1-    dup >r sqrt dup * + r> -    IF  $217 finis THEN
   ;
[ELSE]

   : test-float ;

[THEN]
\ ----------------------------------------------------------------------

$5A5 Constant ovfl-pattern

: test-overflow  ( -- )
   #signbit dup + drop ovfl? 0=                                    IF  $C0 finis THEN
   0 0 + drop ovfl?                                                IF  $C1 finis THEN
       0     0 + ovfl? IF  drop ovfl-pattern  THEN                 IF  $C2 finis THEN
   #7FFF #7FFF - ovfl? IF  drop ovfl-pattern  THEN                 IF  $C3 finis THEN
   #8000 #8000 - ovfl? IF  drop ovfl-pattern  THEN                 IF  $C4 finis THEN
   #8000     1 + ovfl? IF  drop ovfl-pattern  THEN  #8001 -        IF  $C5 finis THEN
       1 #8000 + ovfl? IF  drop ovfl-pattern  THEN  #8001 -        IF  $C6 finis THEN
   #8000     1 - ovfl? IF  drop ovfl-pattern  THEN  ovfl-pattern - IF  $C7 finis THEN
       1 #8000 - ovfl? IF  drop ovfl-pattern  THEN  ovfl-pattern - IF  $C8 finis THEN
   #8000 #7FFF + ovfl? IF  drop ovfl-pattern  THEN  1+             IF  $C9 finis THEN
   #7FFF #8000 + ovfl? IF  drop ovfl-pattern  THEN  1+             IF  $CA finis THEN
   #8000    -1 + ovfl? IF  drop ovfl-pattern  THEN  ovfl-pattern - IF  $CB finis THEN
      -1 #8000 + ovfl? IF  drop ovfl-pattern  THEN  ovfl-pattern - IF  $CC finis THEN
   #8000 #8000 + ovfl? IF  drop ovfl-pattern  THEN  ovfl-pattern - IF  $CD finis THEN
;
: test-memory  ( -- )
   1 2   Location
   st 1 + st  -1 + ld  1 + ld drop  2* - IF  $30 finis THEN
   $10 Location +! Location @ $12 -      IF  $31 finis THEN
   0 Location !
   1 Location +!  Location @ 1-          IF  $32 finis THEN
   -1 Location +! Location @             IF  $33 finis THEN
[ H data_addr_width cache_addr_width u> T ] [IF]
   #8001 #extern st @ #8001 -            IF  $34 finis THEN
   -1 #extern +!   #extern @ #8000 -     IF  $35 finis THEN
    1 #extern +!   #extern @ #8001 -     IF  $36 finis THEN
[THEN]
;
: modify  ( n -- /n )  0= ;

: absbranch  ( n -- )   ;

: test-call  ( -- )
  Status @ #ovfl or  Status ! ovfl?  0= IF  $40 finis THEN
  Status @ #ovfl xor Status ! ovfl?     IF  $41 finis THEN
  Status @ #c    or  Status ! carry? 0= IF  $42 finis THEN
  Status @ #c    xor Status ! carry?    IF  $43 finis THEN
  -1 ['] modify    noop JSR             IF  $44 finis THEN
     ['] absbranch noop BRANCH              $45 finis
;
: test-stack ( -- )
   $55 dup +   $55 2* -                   IF  $50 finis THEN
   1 2 swap 1 - >r 2 - r> or              IF  $51 finis THEN
   1 2 over 1 - >r 2 - >r 1 - r> or r> or IF  $52 finis THEN
   1 2 3 drop 2 - swap 1 - or             IF  $53 finis THEN
   1 2 5 nip + 6 -                        IF  $54 finis THEN
   &55 0 noop ?dup +   &55 -
   &55 0      ?dup +   &55 - or           IF  $55 finis THEN
   &55 1      ?dup + + &57 -
   &55 1 noop ?dup + + &57 - or           IF  $56 finis THEN
   0 >r r@         r>    +                IF  $57 finis THEN
   1 2 3  rot - -                         IF  $58 finis THEN
   3 2 1 -rot - -                         IF  $59 finis THEN
   1 2 tuck + + 5 -                       IF  $5A finis THEN
   1 2 under + + 4 -                      IF  $5B finis THEN
\   test-stack-depths - not implemented
;
: rsp@  ( -- rsp )  Rsp @ ; \ get Rsp with TOR pushed out to rstack memory

: test-register ( -- )
   1 >r r@ r@ + 2 -                        IF  $60 finis THEN
   $55 r> + $56 -  r@ 1 = or               IF  $61 finis THEN
   Status @  0 Status ! Status @           IF  $62 finis THEN
   dup Status !  Status @ -                IF  $63 finis THEN
   di                 Status @ #ie and     IF  $64 finis THEN
   ei                 Status @ #ie and  0= IF  $65 finis THEN
   1 2 3 4 Dsp @ 1- Dsp ! drop + 3 -       IF  $66 finis THEN
   1 >r 2 >r 3 >r   rsp@ Rsp ! rdrop
   r> r> r> + + 6 -                        IF  $67 finis THEN
;
: test-local  ( -- )
   r@ dup >r   1 l@ -                      IF  $80 finis THEN
   r@   -1 1 l!  r> r@ swap >r
      over 1 l!  1 l@ swap 1+ - -  rdrop   IF  $81 finis THEN
      5 -$80 lst  @  5 -                   IF  $82 finis THEN
;
: rsp-task  ( -- u )  Rsp @ [ rs_addr_width negate ] Literal shift [ #tasks 1- ] Literal and ;

: dsp-task  ( -- u )  Dsp @ [ ds_addr_width negate ] Literal shift [ #tasks 1- ] Literal and ;

: rsp-reg   ( -- u )  Rsp @ 1+ [ #rs-depth 1- ] Literal and ;

: dsp-reg   ( -- u )  Dsp @ [ #ds-depth 1- ] Literal and ;

: rsp-task! ( u -- )  r@ swap rs_addr_width shift  rsp-reg or Rsp ! rdrop >r ;

: dsp-task! ( u -- )  ds_addr_width shift  dsp-reg or dup Dsp ! drop ;

: test-van-neumann ( -- )
[ SIMULATION ] [IF]
   $10 pLD  over >r   \ internal blockRAM
   $11 swap  pST  pLD
   rot swap  pST
   p@ r> -                               IF  $A0 finis THEN
   $11 -                                 IF  $A1 finis THEN
[THEN]
;

7 TRAP: push-$10  ( -- n )       $10 ;
8 TRAP: 4*        ( n1 -- n2 )   2* 2* ;

: test-user  ( -- )
   push-$10 $10 -                        IF  $B0 finis THEN
   $10 push-$10 -                        IF  $B1 finis THEN
   1 4*       4 -                        IF  $B2 finis THEN
   2 4*       8 -                        IF  $B3 finis THEN
;
: test-ctrl  ( -- )
   #c-bitout ctrl?                       IF  $B4 finis THEN
   #c-bitout dup  ctrl !  ctrl?       0= IF  $B5 finis THEN
   #f-bitout flag?                    0= IF  $B6 finis THEN
   #c-bitout dup -ctrl !  ctrl?          IF  $B7 finis THEN
   #f-bitout flag?                       IF  $B8 finis THEN
;
: test-timer ( -- )
   time 1- time?                      0= IF  $B9 finis THEN
   time    time?                      0= IF  $BA finis THEN
   time 2 + time?                        IF  $BB finis THEN
;
: test-sema  ( -- )
   #f-sema flag?                         IF  $BC finis THEN
   #f-sema dup pass   flag? 0=           IF  $BD finis THEN
SIMULATION [IF]
   #f-sema pass     \ psr trap must reset #f-sema, otherwise uCore will hang here
[THEN]
   #f-sema release
;
Variable Intvar

: test-interrupt ( -- )   1 Intvar !   ei   #i-ext int-enable ;

: interrupt  ( -- )   intflags
   #i-ext and IF  Intvar @ 1- Intvar !   EXIT THEN
   $55 Intvar !  \ invalid interrupt source
;
Variable save-DSP

: coretest  ( -- )
   1 dup >r 2 dup >r 3 dup >r 0
   test-interrupt
   rsp@   3 rsp-task!
   2dup Dsp @ save-DSP !
   2 dsp-task!
   save-DSP @
     test-register
     test-stack
     test-call
     test-branches
     test-unary
     test-arith
     test-float
     test-overflow
     test-memory
     test-local
     test-user
     test-ctrl
   Dsp ! 2drop  Rsp ! rdrop
   test-timer
   test-van-neumann
   test-sema
   + - +        IF  $F0 finis  THEN
   r> r> r> + - IF  $F1 finis  THEN
SIMULATION [IF]
   Intvar @     IF  $F2 finis  THEN
   #c-bitout ctrl !
   0 finis
[THEN]
   $100 finis
;
