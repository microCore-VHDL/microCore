\ ----------------------------------------------------------------------
\ @file : float_lib.fs
\ ----------------------------------------------------------------------
\
\ Last change: KS 29.06.2023 00:53:41
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
\ @brief : microCore Floating Point Package.
\
\          Identical float wordsets are compiled on the target as well as
\          on the host. Therefore, while interpreting source code during
\          target compilation, floating point operations may be performed
\          in order to compute constants or in line literals.
\
\          Floating point numbers are data_width wide and therefore,
\          they live on the normal data stack.
\          The file hfloat.fs can be used on the host in order to
\          analyze computation stability under different data_width and
\          exp_width conditions. 
\
\          Input and output are done using normal integers, which can be
\          scaled using micro, milli, kilo and mega.
\
\          >float, float> and normalize are realized as uCore instructions,
\          see uCntrl.vhd.
\          In case of an overflow, the largest possible number will be
\          returned and the ovfl status bit will be set. In case of an
\          underflow, + or - zero will be returned and the unfl status bit
\          will be set.
\          Floating point -zero is a zero with the sign bit set, i.e.
\          $80000000 in a 32 bit system.
\
\          Transcendental functions have to be explicitly included:
\          f_exp_log.fs and f_sin_cos.fs.
\
\          cell_width = word width of the host system
\          data_width = word width of the target system
\          exp_width  = width of the target's exponent field
\
\          Object code = 413 instructions including f_exp_log.fs
\
\ Version Author   Date       Changes
\   210     ks   14-Jun-2020  initial version
\ ----------------------------------------------------------------------
Host

cell_width data_width -      Constant #delta_width \ cell width >= data width
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

: normalized?  ( m -- f )   dup #signbit and swap #signbit u2/ and 2* xor ;

: normalize    ( m e -- m' e' )
   over normalized? ?EXIT
   over 0= IF  drop   #exp_min  EXIT THEN
   BEGIN  dup #exp_min = ?EXIT
          1 -   swap 2* swap   over normalized?
   UNTIL
;
Variable underflow  0 underflow !
Variable overflow   0 overflow !

: round   ( dm -- m' )
   over 0< 0= IF  nip  EXIT THEN   \ < 0.5
   swap 2*    IF  1+   EXIT THEN   \ > 0.5
   dup 1 and +                     \ = 0.5, round to even
;
: *.   ( n1 x -- n2 )     over abs um* round swap 0< IF  negate  THEN ;

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
   dup #exp_mask and   ?dup 0= IF  #exp_min  EXIT THEN                                \ de-normalized
   dup #exp_sign and IF  #exp_mask 2/ and  ELSE  #exp_mask 2/ not or  THEN  swap      \ flip sign and extend
   dup 0< IF  #exp_mask 2/ or  2/ [ #signbit #exp_sign or u2/ not ] Literal and       \ add 0.5 for better rounding
        ELSE  #man_mask   and u2/ [ #signbit #exp_sign or u2/     ] Literal or        \ add 0.5 for better rounding
        THEN  swap
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
: fnegate ( r -- -r )       float> swap negate swap >float ;

: f-   ( r1 r2 -- r3 )      fnegate f+ ;

: f*   ( r1 r2 -- r3 )
   float>   rot float>           \ m2 exp2 m1 exp1
   rot + data_width + -rot m*    \ exp3 m2 m1
   #delta_width 0 ?DO  d2*  LOOP
   nip   swap >float
;
: f/   ( r1 r2 -- r3 )   overflow off
   dup 2* 0= IF  not xor #signbit and not  overflow on  EXIT THEN \ leave +/- largest number on / by zero
   float>   rot float>
   data_width 2 - - rot - -rot
   0 swap #delta_width 2 +  0 ?DO  d2/  LOOP
   rot m/mod nip   swap >float
;
: fabs    ( r -- |r| )      dup 0< IF  fnegate  THEN ;
: f<      ( r1 r2 -- f )    f- 0< ;
: f>      ( r1 r2 -- f )    swap f- 0< ;
: f<=     ( r1 r2 -- f )    f> 0= ;
: f>=     ( r1 r2 -- f )    f< 0= ;
: f0=     ( r -- f )        2* 0= ;
: f0<     ( r -- f )        0< ;
: f2*     ( r1 -- r2 )      float> 1+ >float ;
: f2/     ( r1 -- r2 )      float> 1- >float ;

: float   ( n -- r )        0 >float ;

: integer ( r -- n )
   dup 2* #data_mask and 0= IF  2*  EXIT THEN  \ +/- fzero
   1 -1 >float f+                              \ add 0.5 for rounding
   float> ashift
;
: 1/f     ( r1 -- r2 )      1 float swap f/ ;

: fscale  ( r1 n -- r2 )    dup 0< IF  abs float f/  EXIT THEN  float f* ;
: milli   ( r1 -- r2 )      -&1000 fscale ;
: micro   ( r1 -- r2 )      -&1000000 fscale ;
: kilo    ( r1 -- r2 )      &1000 fscale ;
: mega    ( r1 -- r2 )      &1000000 fscale ;

: f.      ( r -- )          integer . ;

Variable Scale  \ used for optimal scaling of a set of polynomial coefficients

: scaled     ( n -- n' )  s>d data_width 1 - 0 DO  d2*  LOOP Scale @ fm/mod nip ;
: scale-factor  ( n -- )  Scale ! ;

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
   2* log2 u2/ [ data_width 1 - negate ] Literal >float f+
;
: exp2   ( ufrac -- uexp2 )  \ Hart 1042, 23 bit precision, 1 > ufrac > 0, 1 = 2**(cell_width-1)
   [ &001877576 &008989340 + &055826318 + &240153617 + &693153073 + &999999925 + scale-factor ]
   >r [ &001877576 scaled ] Literal   r@ *.
      [ &008989340 scaled ] Literal + r@ *.
      [ &055826318 scaled ] Literal + r@ *.
      [ &240153617 scaled ] Literal + r@ *.
      [ &693153073 scaled ] Literal + r> *.
      [ &999999925 scaled ] Literal +
;
: int.frac  ( r -- frac int )  \ split float number into integer and fractional part
   float> [ data_width 2 - ] Literal +
   dup 0< IF  1+ shift  2* 0  EXIT THEN
   0 swap [ #delta_width 2 + ] Literal + 0 DO  d2*  LOOP
;
: +fexp2 ( r1 -- r2 )   int.frac 2** float swap exp2 [ data_width 2 - negate ] Literal >float f* ;

: fexp2  ( r1 -- r2 )   dup f0< IF  fnegate +fexp2 1/f  EXIT THEN  +fexp2 ;

&1442695 float micro Constant log2(e)

: fln    ( r1 -- r2 )   ?fzero flog2 log2(e) f/ ;

: fexp   ( r1 -- r2 )   log2(e) f* fexp2 ;

\ ----------------------------------------------------------------------
\ Sine, Cosine
\ ----------------------------------------------------------------------


: sin ( ufrac --- usin ) \ HART 3341  27 bit precision, pi/2 > frac >= 0, 1 = 2**(cell_width-2)
   [ &000151485 -&004673767 + &079689679 + -&645963711 + &1570796318 + 2* scale-factor ]
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

: degree ( deg -- frad )   &360 mod float [ fpi/2 &90 float f/ ] Literal f* ;

: ?numrange ( -- )   Debugging @ -&24 and throw ;

T definitions

H data_width 1- 2**          T Constant #signbit
H exp_width 2** 1-           T Constant #exp_mask
H #exp_mask not              T Constant #man_mask
H #exp_mask 2/ not           T Constant #exp_min
H #exp_mask 2/ #exp_mask xor T Constant #exp_sign
H 0                          T Constant #fzero_pos
H #signbit                   T Constant #fzero_neg
H #signbit #exp_mask or      T Constant #fmax_neg
H #signbit not               T Constant #fmax_pos
H log2(e)                    T Constant log2(e)
H fpi/2                      T Constant fpi/2

H : scale-factor  ( n -- )     Debugging @ IF  ." can not be loaded in debug mode"  -&24 throw THEN  Scale ! ;
H : scaled        ( n -- n' )  scaled ;

Target

~ : f+   ( r1 r2 -- r3 )
     float>   rot float>   rot   2dup -                                     \ m2 m1 e1 e2 e1-e2
     dup 0< IF  swap >r nip  ELSE  rot >r nip >r swap r> negate  THEN       \ m> m< diff_e1-e2
     1- dup [ data_width exp_width - negate ] Literal u< IF  drop 0 swap  THEN
     over IF  ashift  ELSE  drop  THEN  swap 2/ +   r> 1+ >float
  ;
  Host: f+  ( r1 r2 -- r3 )  comp? dbg? or IF  T f+ H  EXIT THEN  f+ ; immediate
  
~ : fnegate ( r -- -r )      float> swap negate swap >float ;
  Host: fnegate ( r -- -r )  comp? dbg? or IF  T fnegate H  EXIT THEN  fnegate ; immediate
  
  : f-      ( r1 r2 -- r3 )  fnegate f+ ;
  Host: f-  ( r1 r2 -- r3 )  comp? dbg? or IF  T f- H  EXIT THEN  f- ; immediate

~ : f*   ( r1 r2 -- r3 )
     float>   rot float>         \ m2 exp2 m1 exp1
     rot + data_width + -rot     \ m2 m1   R: exp3
     m* nip   swap >float
  ;
  Host: f*  ( r1 r2 -- r3 )  comp? dbg? or IF  T f* H  EXIT THEN  f* ; immediate
  
~ : f/   ( r1 r2 -- r3 )   #ovfl st-reset
     dup 2* 0= IF  not xor #signbit and not  #ovfl st-set  EXIT THEN \ leave +/- largest number on / by zero
     float>   rot float>
     data_width 2 - - rot - -rot
     -2 ashift swap 0 -rot m/mod nip
     swap >float
  ;
  Host: f/  ( r1 r2 -- r3 )  comp? dbg? or IF  T f/ H  EXIT THEN  f/ ; immediate
  
~ : fabs    ( r -- |r| )      dup 0< IF  fnegate  THEN ;
  Host: fabs ( r -- |r| )     comp? dbg? or IF  T fabs H  EXIT THEN  fabs ; immediate

~ : f<      ( r1 r2 -- f )    f- 0< ;
~ : f>      ( r1 r2 -- f )    swap f< ; 
~ : f<=     ( r1 r2 -- f )    f> 0= ;
~ : f>=     ( r1 r2 -- f )    f< 0= ;
~ : f0=     ( r -- f )        2* 0= ;
~ Macro: f0<   ( r -- f )     T 0< H ;

~ : f2*     ( r1 -- r2 )      float> 1+ >float ;
  Host: f2* ( r1 -- r2 )      comp? dbg? or IF  T f2* H  EXIT THEN  f2* ; immediate

~ : f2/     ( r1 -- r2 )      float> 1- >float ;
  Host: f2/ ( r1 -- r2 )      comp? dbg? or IF  T f2/ H  EXIT THEN  f2/ ; immediate

~ Macro: float ( n -- r )     ?comp 0 lit, T >float H ;
  Host: float  ( n -- r )     comp? dbg? or IF  T float H  EXIT THEN  float ; immediate

~ : integer ( r -- n )
     dup 2* 0= IF  2*  EXIT THEN   \ +/- fzero
     1 -1 >float f+                \ add 0.5 for rounding
     float> ashift
  ;
  Host: integer  ( r -- n )   comp? dbg? or IF  T integer H  EXIT THEN  integer ; immediate

~ : 1/f     ( r1 -- r2 )      1 float swap f/ ;
  Host: 1/f ( r1 -- r2 )      comp? dbg? or IF  T 1/f H  EXIT THEN  1/f ; immediate
  
~ : fscale  ( r1 n -- f2 )    dup 0< IF  abs float f/  EXIT THEN  float f* ;
  Host: fscale ( r1 n -- r2 ) comp? dbg? or IF  T fscale H EXIT THEN  fscale ;

~ : milli   ( r1 -- r2 )      -&1000 fscale ;
  Host: milli  ( r1 -- r2 )   comp? dbg? or IF  T milli H  EXIT THEN  milli ; immediate

~ : micro   ( r1 -- r2 )      milli milli ;
  Host: micro  ( r1 -- r2 )   comp? dbg? or IF  T micro H  EXIT THEN  micro ; immediate

~ : kilo    ( r1 -- r2 )      &1000 fscale ;
  Host: kilo   ( r1 -- r2 )   comp? dbg? or IF  T kilo H  EXIT THEN  kilo ; immediate

~ : mega    ( r1 -- r2 )      kilo kilo ;
  Host: mega   ( r1 -- r2 )   comp? dbg? or IF  T mega H  EXIT THEN  mega ; immediate
  
~ : f.      ( r -- )          integer . ;
  Host: f.  ( r -- )          comp? dbg? or IF  T f. H  EXIT THEN  f. ;
  
~ : ?fzero ( r -- r / rdrop !! )   dup 2* ?EXIT  drop #fmax_neg   #ovfl st-set   rdrop ;
  
  : flog2  ( r1 -- r2 )  \ only defined for positive values
     ?fzero float> [ data_width 2 - ] Literal + 0 >float   swap
     2* log2 u2/ [ data_width 1 - negate ] Literal >float f+
  ;
  Host: flog2  ( r1 -- r2 )  comp? dbg? or IF  T flog2 H  EXIT THEN  flog2 ; immediate
  
  : fln    ( r1 -- r2 )     ?fzero flog2 log2(e) f/ ;
  Host: fln  ( r1 -- r2 )   comp? dbg? or IF  T fln H  EXIT THEN  fln ; immediate
  
~ : exp2   ( frac -- exp2[frac] )  \ Hart 1042, 23 bit precision, 1 > frac > 0, 1 = 2**data_width
     [ &001877576 &008989340 + &055826318 + &240153617 + &693153073 + &999999925 + scale-factor ]
     >r [ &001877576 scaled ] Literal   r@ *.
        [ &008989340 scaled ] Literal + r@ *.
        [ &055826318 scaled ] Literal + r@ *.
        [ &240153617 scaled ] Literal + r@ *.
        [ &693153073 scaled ] Literal + r> *.
        [ &999999925 scaled ] Literal +
  ;
  : int.frac  ( r -- frac int )  \ split float number into integer and fractional part
     float> [ data_width 2 - ] Literal +
     dup 0< IF  1+ shift  2*  0  EXIT THEN  0 swap 2 + dshift
  ;
  : +fexp2 ( r1 -- r2 )     int.frac 2** float swap exp2 [ data_width 2 - negate ] Literal >float f* ;
  
  : fexp2  ( r1 -- r2 )     dup f0< IF  fnegate +fexp2 1/f  EXIT THEN  +fexp2 ;
  Host: fexp2 ( r1 -- r2 )  comp? dbg? or IF  T fexp2 H  EXIT THEN  fexp2 ; immediate
  
  : fexp   ( r1 -- r2 )     log2(e) f* fexp2 ;
  Host: fexp ( r1 -- r2 )   comp? dbg? or IF  T fexp H  EXIT THEN  fexp ; immediate
  
  \ ----------------------------------------------------------------------
  \ Converting NTC resistance to/from temperature
  \ ----------------------------------------------------------------------
  
  \ &3892  float Constant B-factor
  \ &10000 float Constant R0
  \ -&298  float Constant -T0
  \ &27300       Constant 0-degC
  \ 
  \ B-factor -T0 f/ R0 fln f+ fexp Constant R-lim
  
  \ : R>T   ( Ohm -- degC*100 )   float R-lim f/   fln   B-factor swap f/   &100 float f* integer 0-degC - ;
  \ 
  \ : T>R   ( degC*100 -- Ohm )   0-degC + float &100 float f/  B-factor swap f/   fexp   R-lim f*   integer ;
  
  \ ----------------------------------------------------------------------
  \ Sine, Cosine
  \ ----------------------------------------------------------------------
  
~ : sin ( ufrac --- usin ) \ HART 3341  27 bit precision, pi/2 > frac >= 0, 1 = 2**(cell_width-2)
     [ &000151485 -&004673767 + &079689679 + -&645963711 + &1570796318 + 2* scale-factor ]
     dup >r  dup *. >r
     [  &000151485 scaled ] Literal   r@ *.
     [ -&004673767 scaled ] Literal + r@ *.
     [  &079689679 scaled ] Literal + r@ *.
     [ -&645963711 scaled ] Literal + r> *.
     [ &1570796318 scaled ] Literal + r> *.
  ;
  : +fsin  ( r1 -- r2 )
     fpi/2 f/   int.frac >r
     r@ 1 and IF  not  THEN  sin
     r> 2 and IF  negate  THEN
     [ data_width 2 - negate ] Literal >float
  ;
  : fsin     ( r -- r' )    dup f0< IF  fnegate +fsin fnegate  EXIT THEN  +fsin ;
  Host: fsin ( r1 -- r2 )   comp? dbg? or IF  T fsin H  EXIT THEN  fsin ; immediate
  
  : fcos     ( r -- r' )    fpi/2 f+ fsin ;
  Host: fcos ( r1 -- r2 )   comp? dbg? or IF  T fcos H  EXIT THEN  fcos ; immediate

~ : degree  ( deg -- frad ) &360 mod float   [ fpi/2 &90 float f/ ] Literal f* ;
  Host: degree ( r1 -- r2 ) comp? dbg? or IF  T degree H  EXIT THEN  degree ; immediate

