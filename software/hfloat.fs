\ ----------------------------------------------------------------------
\ @file : hfloat.fs
\ ----------------------------------------------------------------------
\
\ Last change: KS 05.04.2021 16:47:36
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
\ @brief : Standalone microCore compatible Floating Point Package on the
\          host.
\
\          cell_width = word width of the host system
\          data_width = word width of the target system
\          exp_width  = width of the exponent field
\
\          It can be used to assess floating point precision and function
\          stability experimenting with different combinations of
\          data_width and exp_width.
\
\ Version Author   Date       Changes
\   210     ks   14-Jun-2020  initial version
\ ----------------------------------------------------------------------
Only Forth also definitions

[IFDEF] floating floating [ENDIF] Marker floating

&27 Constant data_width  \ cell width of target system
  7 Constant exp_width   \ width of exponent field

: cell_width ( -- u )    \ cell width of the host Forth system
   0 1 BEGIN  swap 1+ swap  2* ?dup 0= UNTIL
;
cell_width data_width - Constant #delta_width \ cell width (host) must be >= data width (target)
                           
: shift  ( n1 quan -- n2 )    dup 0< IF  abs rshift  EXIT THEN  lshift ;
: ashift ( n1 n2 -- n3 )      dup 0< IF  negate 0 DO 2/ LOOP EXIT THEN  0 ?DO 2* LOOP ;
: u2/    ( u1 -- u2 )         1 rshift ;
: 2**    ( u -- 2**u )        1 swap 0 ?DO 2* LOOP ;
: m/mod  ( d n -- rem quot )  fm/mod ;
' invert Alias not

data_width 1- 2**            Constant #signbit
exp_width 2** 1-             Constant #exp_mask
#exp_mask not                Constant #man_mask
#exp_mask 2/ not             Constant #exp_min
#exp_mask #exp_min and       Constant #exp_sign
#signbit                     Constant #fzero_neg
0                            Constant #fzero_pos
#signbit #exp_mask or        Constant #fmax_neg
#signbit not                 Constant #fmax_pos
-1 #delta_width negate shift Constant #data_mask

Variable underflow  0 underflow !
Variable overflow   0 overflow !

Variable Scale  \ used for optimal scaling of a set of polynomial coefficients
: scaled     ( n -- n' )  s>d data_width 1 - 0 DO  d2*  LOOP Scale @ fm/mod nip ;
: scale_factor  ( n -- )  Scale ! ;

: round   ( dm -- m' )
   over 0< 0= IF  nip  EXIT THEN   \ < 0.5
   swap 2*    IF  1+   EXIT THEN   \ > 0.5
   dup 1 and +                     \ = 0.5, round to even
;
: *.   ( n1 x -- n2 )       over abs um* round swap 0< IF  negate  THEN ;

: normalized?  ( m -- f )   dup #signbit and swap #signbit u2/ and 2* xor ;

: normalize    ( m e -- m' e' )
   over normalized? ?EXIT
   over 0= IF  drop   #exp_min  EXIT THEN
   BEGIN  dup #exp_min = ?EXIT
          1 -   swap 2* swap   over normalized?
   UNTIL
;
: >float  ( m e -- r )
   overflow off   underflow off
   normalize   swap #man_mask and swap
   over #fzero_neg =   over #exp_min =   and >r
   over #fzero_pos =   r> or
   IF  drop  #exp_mask not and  EXIT THEN                \ leave floating +/-zero. For +zero irrespective of exponent
   dup #man_mask 2/ and
   dup 0< IF  #man_mask 2/ xor  THEN                     \ exponent over/underflow?
   IF  0< IF  underflow on   0< IF  #fzero_neg  EXIT THEN  #fzero_pos  EXIT
        THEN   overflow on   0< IF  #fmax_neg   EXIT THEN  #fmax_pos   EXIT
   THEN
   dup #exp_min = IF  drop #man_mask and  EXIT THEN      \ smallest exponent => denormalized
   #exp_mask and   #exp_sign xor   swap                  \ flip sign of exponent => bias = #exp_min
   dup 2* [ #signbit not #exp_mask not and ] Literal and
   swap 0< IF  #signbit or  THEN  or
;
: float>  ( r -- m e )
   dup #exp_mask and   ?dup 0= IF  #exp_min  EXIT THEN                             \ de-normalized
   dup #exp_sign and IF  #exp_mask 2/ and  ELSE  #exp_mask 2/ not or  THEN  swap   \ flip sign and extend
   dup 0< IF  #exp_mask 2/ or  2/ [ #signbit #exp_sign or u2/ not ] Literal and    \ add 0.5 for better rounding
        ELSE  #man_mask   and u2/ [ #signbit #exp_sign or u2/     ] Literal or     \ add 0.5 for better rounding
        THEN  swap
;
: int.frac  ( r -- frac int )  \ split float number into integer and fractional part
   float> [ data_width 2 - ] Literal +
   dup 0< IF  not 0 ?DO  u2/  LOOP  2* 0  EXIT THEN
   0 swap [ #delta_width 2 + ] Literal + 0 DO  d2*  LOOP
;
data_width &32 = [IF]

: >ieee ( r -- ieee )  \ only valid for 32-bit data_width
   float> $80 xor $7F + $FF and                   \ exponent
   over 0< IF  $100 or  THEN  &23 shift swap      \ sign
   abs -&7 shift $7FFFFF and or                   \ mantissa
;
: ieee> ( ieee -- r )  \ only valid for 32-bit data_width
   dup   dup 0< IF  negate $7FFFFF and $1000000 or  ELSE  $7FFFFF and $800000 or  THEN  7 shift
   swap -&23 shift $7F -  dup $80 and IF  $7F and  ELSE  $7F not or  THEN  >float
;
[THEN]

: f+   ( r1 r2 -- r3 )
   float>   rot float>   rot   2dup -                                     \ m2 m1 e1 e2 e1-e2
   dup 0< IF  swap >r nip  ELSE  rot >r nip >r swap r> negate  THEN       \ m> m< diff_e1-e2
   1- dup [ data_width exp_width - negate ] Literal u< IF  drop 0 swap  THEN
   over IF  ashift  ELSE  drop  THEN  swap 2/ +   r> 1+ >float
;
: f*   ( r1 r2 -- r3 )
   float>   rot float>         \ m2 exp2 m1 exp1
   rot + data_width + -rot     \ exp3 m2 m1
   m* #delta_width 0 ?DO  d2*  LOOP
   nip swap >float
;
: f/   ( r1 r2 -- r3 )   overflow off
   dup 2* 0= IF  not xor #signbit and not  overflow on  EXIT THEN \ leave +/- largest number on / by zero
   float>   rot float>
   data_width - rot - -rot
   0 swap #delta_width 2 +  0 ?DO  d2/  LOOP
   rot m/mod nip   swap 2 + >float
;
: fnegate  ( r -- -r )
   dup 2* IF  float> 1+ swap 2/ not #exp_sign 2/ + swap >float  EXIT THEN
   0< IF  0  EXIT THEN  #signbit  \ handle + and - zero
;
: fabs    ( r -- |r| )      dup 0< IF  fnegate  THEN ;
: f-      ( r1 r2 -- r3 )   fnegate f+ ;
: f<      ( r1 r2 -- f )    f- 0< ;
: f>      ( r1 r2 -- f )    swap f- 0< ;
: f<=     ( r1 r2 -- f )    f> 0= ;
: f>=     ( r1 r2 -- f )    f< 0= ;
: f0=     ( r -- f )        2* 0= ;
: f0<     ( r -- f )        0< ;
: f2*     ( r1 -- r2 )      float> 1+ >float ;
: f2/     ( r1 -- r2 )      float> swap 2/ swap >float ;

: float   ( n -- r )        0 >float ;

: integer ( r -- n )
   dup 2* #data_mask and 0= IF  2*  EXIT THEN  \ +/- zero
   1 float f2/ f+                              \ add 0.5 for rounding
   float> ashift
;
: 1/f     ( r1 -- r2 )      1 float swap f/ ;

: fscale  ( r1 n -- f2 )    dup 0< IF  abs float f/  EXIT THEN  float f* ;
: milli   ( r1 -- r2 )      -&1000 fscale ;
: micro   ( r1 -- r2 )      -&1000000 fscale ;
: kilo    ( r1 -- r2 )       &1000 fscale ;
: mega    ( r1 -- r2 )       &1000000 fscale ;

\ ----------------------------------------------------------------------
\ logarithm, exponential
\ ----------------------------------------------------------------------

: log2 ( frac -- log2 )   \ Bit-wise Logarithm (K.Schleisiek/U.Lange)
   #delta_width 0 ?DO  2*  LOOP
   0   data_width 0
   DO  2* >r   dup um*
      dup 0< IF  r> 1+ >r  ELSE  d2*  THEN     \ correction of 'B(i)' and 'A(i)'
      round   r>                               \ A(i+1):=A(i)*2^(B(i)-1)
   LOOP  nip
;
: ?fzero ( r -- r / rdrop !! )   dup 2* #data_mask and ?EXIT  drop #fmax_neg   overflow on   rdrop ;

: flog2  ( r1 -- r2 )  \ only defined for positive values
   ?fzero float> [ data_width 2 - ] Literal + 0 >float   swap
   abs 2* log2 u2/ [ data_width 1 - negate ] Literal >float f+
;
: exp2  ( ufrac -- uexp2 )  \ Hart 1042, 23 bit precision, 1 > ufrac > 0, 1 = 2**(cell_width-1)
   [ &001877576 &008989340 + &055826318 + &240153617 + &693153073 + &999999925 + scale_factor ]
   >r [ &001877576 scaled ] Literal   r@ *.
      [ &008989340 scaled ] Literal + r@ *.
      [ &055826318 scaled ] Literal + r@ *.
      [ &240153617 scaled ] Literal + r@ *.
      [ &693153073 scaled ] Literal + r> *.
      [ &999999925 scaled ] Literal +
;
: +fexp2 ( r1 -- r2 )   int.frac 2** float   swap exp2 [ data_width 2 - negate ] Literal >float f* ;

: fexp2  ( r1 -- r2 )   dup f0< IF  fnegate +fexp2 1/f  EXIT THEN  +fexp2 ;

&1442695 float micro Constant log2(e)

: fln  ( r1 -- r2 )  ?fzero flog2 log2(e) f/ ;

: fexp ( r1 -- r2 )  log2(e) f* fexp2 ;

\ ----------------------------------------------------------------------
\ sine, cosine
\ ----------------------------------------------------------------------

: sin ( ufrac --- usin )           \ HART 3341  27 bit precision, pi/2 > frac >= 0, 1 = 2**(cell_width-2)
   [ &000151485 -&004673767 + &079689679 + -&645963711 + &1570796318 + 2* scale_factor ]
   dup >r  dup *. >r
   [  &000151485 scaled ] Literal   r@ *.
   [ -&004673767 scaled ] Literal + r@ *.
   [  &079689679 scaled ] Literal + r@ *.
   [ -&645963711 scaled ] Literal + r> *.
   [ &1570796318 scaled ] Literal + r> *.
;
&104348 float &33215 2* float f/ Constant fpi/2

: +fsin  ( r1 -- r2 )
   fpi/2 f/   int.frac >r
   r@ 1 and IF  not  THEN  sin
   r> 2 and IF  negate  THEN
   [ data_width 2 - negate ] Literal >float
;
: fsin  ( r -- r' )  dup f0< IF  fnegate +fsin fnegate  EXIT THEN  +fsin ;

: fcos  ( r -- r' )  fpi/2 f+ fsin ;

: degree ( fdeg -- frad )   [ fpi/2 &90 float f/ ] Literal f* ;

\ ----------------------------------------------------------------------
\ Converting NTC resistance to temperature and vice versa
\ ----------------------------------------------------------------------

&3892  float Constant B-factor
-&298  float Constant -T0
&10000 float Constant R0
&27300       Constant 0-degC

B-factor -T0 f/ R0 fln f+ fexp Constant R-lim

: R>T   ( Ohm -- degC*100 )   float R-lim f/   fln   B-factor swap f/   &100 float f* integer 0-degC - ;

: T>R   ( degC*100 -- Ohm )   0-degC + float &100 float f/  B-factor swap f/   fexp   R-lim f*   integer ;

\ : ntc-test  ( -- )   &1000 &50000 bounds DO  cr I 6 .r I r>t dup 6 .r t>r dup 6 .r I - 5 .r  &1000 +LOOP ;
: ntc-test  ( -- )     &8100 -&2000        DO  cr I 6 .r I t>r dup 7 .r r>t dup 6 .r I - 3 .r  &200 +LOOP ;

