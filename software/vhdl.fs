\ ----------------------------------------------------------------------
\ @file : vhdl.fs
\ ----------------------------------------------------------------------
\
\ Last change: KS 01.03.2022 18:48:14
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
\ @brief : A simplistic VHDL interpreter for importing microCore
\          architecture parameters into the cross compiler environment
\          including e.g. bus widths and opcodes. To this end
\          architecture_pkg.vhd will be loaded during cross compilation.
\          Architecture_pkg has been written in a peculiar way so that
\          it can be interpreted by the VHDL simulator, the
\          VHDL synthesizer as well as the Forth cross compiler.
\          When loaded by the cross compiler, code between --~ up to
\          --~-- will be skipped.
\
\ Version Author   Date       Changes
\   210     ks   14-Jun-2020  initial version
\   2200    ks   15-Dec-2020  more literal number formats
\ ----------------------------------------------------------------------
\ E.g. the following VHDL expressions will be interpreted by Forth:
\
\ CONSTANT WITH_MULT   : BOOLEAN   := false;
\ CONSTANT data_width  : NATURAL   := 16;
\ CONSTANT addr_rstack : NATURAL   := 16#C00#;
\ CONSTANT flag_reg    : INTEGER   := -2;
\ CONSTANT op_NOOP     : byte      := "00000000";
\ CONSTANT op_NOOP     : byte      := X"00";
\ CONSTANT video_rate  : REAL      := 55.0;
\ ----------------------------------------------------------------------
Forth definitions

' \ Alias -- immediate

: base>number  ( addr len -- n )  over c@
   [char] ' case? IF  1 /string drop c@ [char] 1 =                    EXIT THEN
   [char] " case? IF  1 /string [char] " token  binary s>number drop  EXIT THEN
   [char] O case? IF  1 /string [char] " token  octal  s>number drop  EXIT THEN
   [char] X     = IF  1 /string [char] " token  hex    s>number drop  EXIT THEN
   2dup   [char] # scan dup                               \ is it a NATURAL with base prefix?
   IF  2>r   [char] # token decimal s>number drop Base !
       2r>   [char] # token s>number drop
   EXIT THEN  2drop                                       \ its a decimal number
   BL token decimal s>number drop
;
: VHDL-number  ( <source> -- n )   Base save   BL skip-input   [char] ; parse base>number ;

: dec_parameter ( <source> -- n )   Base save   BL skip-input   [char] ; parse decimal s>number drop ;

Vocabulary --VHDL   --VHDL definitions

1 Constant STD_LOGIC
1 Constant byte
1 Constant NATURAL
1 Constant INTEGER
2 Constant BOOLEAN
3 Constant REAL

: STD_LOGIC_VECTOR  ( -- type )  postpone ( byte ;    \ )
: UNSIGNED          ( -- type )  postpone ( byte ;    \ )

: CONSTANT  ( -- )   0 Constant ;

: vhdl-types  ( type -- n )
   1 case? IF  VHDL-number                   EXIT THEN
   2 case? IF  [char] ; word count evaluate  EXIT THEN  \ for conditional compilation
   3 case? IF  dec_parameter &10 /           EXIT THEN
   abort" unknown type"
;
: :=      ( type -- )   Base save  vhdl-types   here cell- ! ; \ patch constant created before

: --~     ( ccc~ -- )   [char] ~ scan-input ;         \ ~

: --Forth ( ccc~ -- )   Forth --~ ;

' \       Alias --
' noop    Alias ;
' noop    Alias :

Forth definitions
