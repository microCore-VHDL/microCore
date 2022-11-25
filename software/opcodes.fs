\ ----------------------------------------------------------------------
\ @file : opcodes.fs
\ ----------------------------------------------------------------------
\
\ Last change: KS 02.10.2022 16:31:47
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
\ @brief : Defining Forth names for instructions and macros.
\
\ Version Author   Date       Changes
\   210     ks   14-Jun-2020  initial version
\   2300    ks   12-Feb-2021  compiler switch WITH_PROG_RW eliminated
\   2400    ks   23-Jun-2022  byte and word memory access added using byte_addr_width
\ ----------------------------------------------------------------------
Target

\ stack
op_NOOP        Op: noop    ( -- )             noop
op_SWAP      Swap: swap    ( 1 2 -- 2 1 )     swap  \ optimizes 'swap swap' away
op_DROP        Op: drop    ( n -- )           drop
            Macro: 2drop   ( n n -- )         comp? IF T drop drop H EXIT THEN  2drop ;
            Macro: nip     ( 1 2 -- 2 )       comp? IF T swap drop H EXIT THEN  nip ;
op_DUP         Op: dup     ( n -- n n )       dup
op_QDUP        Op: ?dup    ( n -- 0 | n n )   ?dup
op_OVER        Op: over    ( 1 2 -- 1 2 1 )   over
            Macro: 2dup    ( d -- d d )       comp? IF T over over H EXIT THEN  2dup ;
            Macro: tuck    ( 1 2 -- 2 1 2 )   comp? IF T swap over H EXIT THEN  swap over ;
            Macro: under   ( 1 2 -- 1 1 2 )   comp? IF T over swap H EXIT THEN  over swap ;
op_ROT         Op: rot     ( 1 2 3 -- 3 1 2 ) rot
            Macro: -rot    ( 1 2 3 -- 2 3 1 ) comp? IF T rot rot H EXIT THEN  -rot ;

\ return stack
op_RPUSH      Tor: >r      ( n -- )           don't
            Macro: 2>r     ( d -- ) ( R: -- d )  T >r >r H ;
op_RPOP        Op: r>      ( -- n )           don't
            Macro: 2r>     ( -- d ) ( R: d -- )  T r> r> H ;
op_RTOR        Op: r@      ( -- n )           don't
op_LOCAL       Op: local   ( rel -- addr )    don't

\ data memory
op_LOAD        Op: ld      ( addr -- n addr ) don't
op_STORE       Op: st      ( n addr -- addr ) don't
            Macro: !       ( n addr -- )      comp? dbg? or IF T st drop H EXIT THEN  d! ;
EXTENDED [IF]
   op_FETCH    Op: @       ( addr -- u )      d@
   op_RDROP    Op: rdrop   ( -- )             don't
[ELSE]
            Macro: @       ( addr -- u )      comp? IF T ld drop H EXIT THEN  d@ ;
            Macro: rdrop   ( -- )             ?comp T r> drop H ;
[THEN]
            Macro: lst     ( n rel -- addr )  ?comp T local st H ;
            Macro: l!      ( n rel -- )       ?comp T local ! H ;
            Macro: lld     ( rel -- n addr )  ?comp T local ld H ;
            Macro: l@      ( rel -- n )       ?comp T local @ H ;

byte_addr_width [IF]
   op_CLOAD       Op: cld   ( caddr -- c caddr ) don't
   EXTENDED [IF]
      op_CFETCH   Op: c@    ( caddr -- c )       cd@
   [ELSE]
               Macro: c@    ( caddr -- c )       comp? dbg? or IF T cld drop H EXIT THEN  cd@ ;
   [THEN]
   op_CSTORE      Op: cst   ( c caddr -- caddr ) don't
               Macro: c!    ( c caddr -- )       comp? dbg? or IF T cst drop H EXIT THEN  cd! ;
   byte_addr_width = 2 [IF]
      op_WLOAD    Op: wld   ( caddr -- w caddr ) don't
               Macro: w@    ( caddr -- w )       ?comp T wld drop H ;
      op_WSTORE   Op: wst   ( w caddr -- caddr ) don't
               Macro: w!    ( w caddr -- )       ?comp T wst drop H ;
   [THEN]
[THEN]
\ program memory
op_PLOAD       Op: pLD    ( addr -- b addr )  don't
            Macro: p@     ( addr -- b )       ?comp T pLD drop H ;
op_PSTORE      Op: pST    ( b addr -- addr )  don't
            Macro: p!     ( b addr -- )       ?comp T pST drop H ;

\ branch, call and exit
op_BRANCH     Brn: branch     ( addr -- )     don't  \ ELSE, REPEAT
op_QBRANCH    Brn: 0=branch   ( addr f -- )   don't  \           IF
op_NEXT       Brn: tor-branch ( addr -- )     don't  \ NEXT
op_CALL        Op: JSR     ( -- )             don't
            Macro: execute ( addr -- )        ?comp ?noop, T JSR H ;
op_EXIT        Ex: exit    ( -- )             don't
op_IRET        Op: iret    ( status -- )      don't
op_BREAK       Op: break   ( -- )             don't

\ unary arithmetic
op_NOT         Op: invert  ( u1 -- u2 )       not
op_NOT         Op: not     ( u1 -- u2 )       not
op_ZEQU        Op: 0=      ( n -- f )         0=
            Macro: 0<>     ( n -- f )         comp? IF  T 0= 0= H EXIT THEN  0= 0= ;
op_ZLESS       Op: 0<      ( n -- f )         0<
            Macro: extend  ( n -- d )         comp? IF  T dup 0< H EXIT THEN  dup 0< ;

\ binary arithmetic
op_ADD      Arith: +       ( n1 n2 -- n3 )    +
op_ADC      Arith: +c      ( n1 n2 -- n3 )    don't
op_SUB        Sub: -       ( n1 n2 -- n3 )    -
op_SSUB     Arith: swap-   ( n2 n1 -- n3 )    don't
            Macro: negate  ( n -- -n )        comp? IF  0 lit, T swap- H EXIT THEN  negate ;
op_AND      Arith: and     ( u1 u2 -- u3 )    and
op_OR       Arith: or      ( u1 u2 -- u3 )    or
op_XOR      Arith: xor     ( u1 u2 -- u3 )    xor

\ shifting
WITH_MULT [IF]
   op_MSHIFT   Op: mshift  ( u n -- ud )      don't
            Macro: shift   ( u n -- u' )      comp? IF  T mshift drop  H EXIT THEN shift ;
            Macro: rotate  ( n1 n2 -- n3 )    ?comp T mshift or H ;
   op_MASHIFT  Op: mashift ( n1 n2 -- d )     don't
            Macro: ashift  ( n1 n2 -- n1' )   comp? IF  T mashift drop H EXIT THEN ashift ;
[ELSE]
   op_MASHIFT  Op: ashift  ( n1 n2 -- n1' )   ashift
   op_MSHIFT   Op: shift   ( u n -- u' )      shift
   op_SRC      Op: c2/     ( u -- u' )        don't
   op_SLC      Op: c2*     ( u -- u' )        don't
[THEN]
            Macro: 2*      ( u1 -- u2 )       comp? IF  T dup +          H EXIT THEN 2* ;
            Macro: 2/      ( n1 -- n2 )       comp? IF  -1 lit, T ashift H EXIT THEN 2/ ;
            Macro: u2/     ( u1 -- u2 )       comp? IF  -1 lit, T shift  H EXIT THEN u2/ ;

\ Flags
op_OVFLQ       Op: ovfl?   ( -- f )           don't
op_CARRYQ      Op: carry?  ( -- f )           don't
op_TIMEQ       Op: time?   ( time -- f )      don't
op_LESS        Op: <       ( n1 n2 -- f )     <
            Macro: >=      ( n1 n2 -- f )     comp? IF  T < 0=   H EXIT THEN  < 0= ;
            Macro: >       ( n1 n2 -- f )     comp? IF  T swap < H EXIT THEN  > ;
            Macro: <=      ( n1 n2 -- f )     comp? IF  T > 0=   H EXIT THEN  > 0= ;
            Macro: =       ( n1 n2 -- f )     comp? IF  T - 0=   H EXIT THEN  = ;
            Macro: /=      ( n1 n2 -- f )     comp? IF  T - 0<>  H EXIT THEN  - 0<> ;
            Macro: d0=     ( d -- f )         comp? IF  T or 0=  H EXIT THEN  or 0= ;

\ complex arithmetic
WITH_MULT [IF]
   op_UMULT    Op: um*     ( u1 u2 -- ud )    don't
   op_SMULT    Op: m*      ( n1 n2 -- d )     don't
   op_MULTL    Op: multl   ( ud -- n )        don't
            Macro: *       ( n1 n2 -- n3 )    comp? dbg? or IF T m* multl H EXIT THEN  * ;
[ELSE] \ without MULT
   op_UMULT    Op: mults   ( complex )        don't
   op_MULTL    Op: multl   ( ud -- n )        don't
            Macro: umultiply ( u1 u2 -- ud )  ?comp 0 lit, data_width 0 DO T mults H LOOP T -rot swap drop H ;
[THEN]
op_DIV         Op: div     ( complex )        don't
op_UDIVS       Op: udivs   ( complex )        don't
op_UDIVL       Op: udivl   ( complex )        don't
            Macro: udivide ( ud div -- urem uquot )  ?comp T udivs H data_width 0 DO T div H LOOP T udivl H ;

\ status register
op_STSET       Op: st-set      ( mask -- )    don't
            Macro: st-reset    ( mask -- )    ?comp T not st-set H ;

EXTENDED [IF]
 
   op_INDEX       Op: I       ( -- i )               don't
   op_PLUSST      Op: +st     ( n addr -- addr )     don't   \ indivisible read-modify-write instruction
               Macro: +!      ( n addr -- )          comp? IF T +st drop H EXIT THEN  +! ;
   op_FLAGQ       Op: flag?   ( mask -- f )          don't
   op_NZEXIT      Op: nz-exit ( f -- )               don't
   op_ADDSAT      Op: +sat    ( n1 n2 -- n3 )        don't
   op_SDIVL       Op: sdivl   ( complex )            don't
   op_SDIVS       Op: sdivs   ( complex )            don't
               Macro: sdivide ( d div -- rem quot )  ?comp T sdivs H data_width 0 DO T div H LOOP T sdivl H ;
   op_SQRTS       Op: sqrts   ( complex )            don't
   data_width 1 and [IF]                            
      op_SQRT0    Op: sqrt0   ( complex )            don't
               Macro: uroot   ( u -- rem root )      ?comp 0 lit, T tuck sqrt0 H data_width 2/ 0 DO T sqrts H LOOP T -rot drop H ;
   [ELSE]                                           
               Macro: uroot   ( u -- rem root )      ?comp 0 lit, T tuck       H data_width 2/ 0 DO T sqrts H LOOP T -rot drop H ;
   [THEN]

[ELSE]

               Macro: flag?   ( mask -- f )          ?comp FLAG_REG lit, T @ and H ;

[THEN]

\ floating point operators
WITH_FLOAT [IF]

   WITH_MULT [IF]  \ with floating point operators
      op_FMULT Op: *.      ( n1 x -- n2 )     don't
      op_LOGS  Op: log2s   ( u  0  -- u' ld ) don't
            Macro: ulog    ( u -- ld )        ?comp 0 lit, data_width 0 DO T log2s H LOOP T swap drop H ;
   [THEN]
   op_NORM     Op: normalize ( n e -- n' e' ) don't
   op_FLOAT    Op: (>float ( n exp -- r )     don't
            Macro: >float  ( n exp -- r )     ?comp T normalize (>float H ;
   op_INTEG    Op: float>  ( r -- n exp )     don't

[THEN]

\ some macros

Macro: pause   ( -- )             T noop H ; \ this pause will be used when the multitasker is not loaded.
         \ Pause compiles a noop just in case debugger commands upload or download will be used.
         \ The noop will allow data memory access by the umbilical interface without interfering
         \ with data memory access of the running program.

Macro: true    ( -- tf )          comp? IF -1 lit,  EXIT THEN  true ;
Macro: false   ( -- ff )          comp? IF  0 lit,  EXIT THEN  false ;

Macro: 1+      ( n -- n' )        comp? IF   1 lit, T + H EXIT THEN  1+ ;
Macro: 1-      ( n -- n' )        comp? IF  -1 lit, T + H EXIT THEN  1- ;

Macro: cells   ( u -- u' )        #cell comp? IF  lit, T * H EXIT THEN  * ;
Macro: cell+   ( u -- u' )        #cell comp? IF  lit, T + H EXIT THEN  + ;
Macro: cell-   ( u -- u' )        #cell comp? IF  lit, T - H EXIT THEN  - ;

Macro: ei          ( -- )         ?comp s_ie 2**     lit, T st-set H ; \ 1 -> #ie bit
Macro: di          ( -- )         ?comp s_ie 2** not lit, T st-set H ; \ 0 -> #ie bit

Macro: int-enable  ( mask -- )    ?comp INT_REG lit, T ! H ;
Macro: int-disable ( mask -- )    ?comp T not H INT_REG lit, T ! H ;

Macro: pass    ( n -- )           ?comp         FLAG_REG lit, T ! H ;  \ lock semaphor bit
Macro: release ( n -- )           ?comp T not H FLAG_REG lit, T ! H ;  \ release semaphor bit

Macro: host@   ( -- u )           ?comp DEBUG_REG lit, T @ H ;
Macro: host!   ( u -- )           ?comp DEBUG_REG lit, T ! H ;

Macro: ctrl    ( -- reg )         ?comp         CTRL_REG lit, ;
Macro: -ctrl   ( n -- -n reg )    ?comp T not H CTRL_REG lit, ;

Macro: >rstack ( rsp -- )         ?comp RSP_REG lit, T ! rdrop H ;
Macro: dstack> ( -- dsp )         ?comp DSP_REG lit, T dup @ nip H ;
Macro: >dstack ( dsp -- )         ?comp DSP_REG lit, T ! drop H ;

Macro: time    ( -- time )        ?comp TIME_REG lit, T @ H ;
Macro: ahead   ( ticks -- time )  ?comp T time + H ;
