\ ----------------------------------------------------------------------
\ @file : images.fs
\ ----------------------------------------------------------------------
\
\ Last change: KS 24.03.2021 17:52:02
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
\ @brief : Generating the target object code image in different formats.
\
\ Version Author   Date       Changes
\   210     ks   14-Jun-2020  initial version
\   2300    ks   06-Feb-2021  CRC-file modified for variable data_widths
\ ----------------------------------------------------------------------
Only Forth also definitions

: Bin-file   ( <name> -- ) \ write program memory image to file <name> as binary
   BL word dup c@ 0= abort" file name required"
   count R/W BIN           Create-file abort" File could not be created"  >r
   0 >memory there cells r@ write-file abort" File write error"
   r>                       close-file abort" File close failed"
;
: Byte-file   ( <name> -- ) \ write program memory image to file <name> as byte oriented binary
   BL word dup c@ 0= abort" file name required"
   count R/W BIN  Create-file abort" File could not be created"
   there 0 ?DO I >memory @  pad c!  dup   pad 1 rot write-file abort" File write error"  LOOP
   close-file abort" File close failed"
;
\ ----------------------------------------------------------------------
\ Create bootable code for 8-bit EEPROM
\ length-field | program image | 2 byte crc
\ ----------------------------------------------------------------------

$1021 Constant #polynom
$FFFF Constant #crc-init
$8000 Constant #X15-bit
$FFFF Constant #crc-mask
    2 Constant #bytes/crc

: crc-step  ( w crc1 -- w' crc2 ) \ process one bit
   2dup xor >r   2* swap 2* swap
   r> #X15-bit and IF  #polynom xor  THEN
;
: crc8  ( crc1 c -- crc2 )  \ process one byte
   8 lshift swap &7 FOR  crc-step  NEXT  nip
;
: crc-check ( addr length -- crc )  \ process string of bytes
   #crc-init -rot 1- FOR  dup >r   c@ crc8   r> 1+  NEXT  drop
   #crc-mask and
;
: pack_image ( -- addr len )
   Pad    dup #bytes/cell + 0
   0 >memory there cells bounds
   DO  I c@   swap >r swap >r   r@ c!  r> 1+  r> 1+  1 cells +LOOP
   swap >r   over #bytes/cell + over crc-check
   unpack r@ c!  r> 1+ c!                                         \ append two CRC bytes
   swap over #bytes/cell 2 - FOR  unpack  NEXT                    \ fill in length-field
   Pad #bytes/cell + 1-   #bytes/cell 1- FOR  tuck c! 1-  NEXT drop
   swap #bytes/cell + #bytes/crc +                                \ compute total image length
;
: CRC-file   ( <name> -- )  \ write program memory image length field and CRC
   BL word          dup c@ 0= abort" file name required"
   count R/W BIN  Create-file abort" File could not be created" >r
   pack_image   r@ write-file abort" File write error"
   r> close-file              abort" File close failed"
;
\ ----------------------------------------------------------------------
\ Create VHDL output files for the object code
\ ----------------------------------------------------------------------

: makeopcode ( opcode -- string )
   Base save binary  0 <# inst_width 0 DO # LOOP #>
;
: writeclause  ( handle addr -- )
   count rot write-file abort" File write error"
;
: file>here  ( string -- addr len )
   count r/o open-file abort" PROLOG/EPILOG file not present" >r
   here 2000 r@ read-file abort" PROLOG/EPILOG: could not read"
   here swap   r> close-file drop
;
\ ----------------------------------------------------------------------
\ Create VHDL program memory code using case statement
\ ----------------------------------------------------------------------

Create romclause ,"       WHEN 16#0000# => RETURN _00000000_;  "

: >romclause  ( offset -- addr )  romclause + 6 + ;

char " $19 >romclause c!
char " $22 >romclause c!
   #cr $24 >romclause c!
   #lf $25 >romclause c!

  9 Constant vhdladdr
$1A Constant vhdlcode

: makeaddr ( addr -- string )    temp-hex  0 <# # # # # #> ;

: make_whenclause  ( caddr -- )
   dup makeaddr vhdladdr >romclause swap cmove
   >memory @ makeopcode vhdlcode >romclause swap cmove
;
Create prolog ," prolog.vhd"
Create epilog ," epilog.vhd"

: VHDL-file  ( <name> -- )
   BL word dup c@ 0= abort" file name required"
   count R/W BIN create-file abort" File could not be created" >r
   prolog file>here r@ write-file abort" PROLOG write error"
   r> 0 there bounds  DO  I make_whenclause  dup romclause writeclause  LOOP  >r
   epilog file>here r@ write-file abort" EPILOG write error"
   r> close-file abort" File close failed"
;
Target definitions Forth

' VHDL-file Alias VHDL-file

definitions

\ ----------------------------------------------------------------------
\ Create VHDL boot loader rom code using case statement
\ ----------------------------------------------------------------------

Create boot_prolog ," prolog_boot.vhd"

: Boot-file  ( <name> -- )
   BL word dup c@ 0= abort" file name required"
   count R/W BIN create-file abort" File could not be created" >r
   boot_prolog file>here r@ write-file abort" PROLOG write error"
   r> 0 there bounds  DO  I make_whenclause  dup romclause writeclause  LOOP  >r
   epilog file>here r@ write-file abort" EPILOG write error"
   r> close-file abort" File close failed"
;
\ ----------------------------------------------------------------------
\ Create VHDL blockRAM memory
\ ----------------------------------------------------------------------

Create clause ," _00000000_,  "

: >clause  ( offset -- addr )  clause + ;

char " $1 >clause c!
char " $A >clause c!
   #cr $C >clause c!
   #lf $D >clause c!

$2 Constant vhdlcode

: make_initstring  ( caddr -- )
   >memory @ makeopcode vhdlcode >clause swap cmove
;
Create internal_prolog ," prolog_internal.vhd"
Create internal_epilog ," epilog_internal.vhd"

: Prog-file  ( <name> -- )
   BL word dup c@ 0= abort" file name required"
   count R/W BIN create-file abort" File could not be created" >r
   internal_prolog file>here r@ write-file abort" PROLOG write error"
   r> 0 there bounds  DO  I make_initstring  dup Clause writeclause  LOOP  >r
   internal_epilog file>here r@ write-file abort" EPILOG write error"
   r> close-file abort" File close failed"
;
\ ----------------------------------------------------------------------
\ Create Xilinx MEM File for block ram initialization
\ ----------------------------------------------------------------------

: placeaddr ( addr paddr -- addr paddr+ )
\  >r dup 0 <#                       # # # # [char] @ hold #> r> 2dup + >r swap cmove r>  \ Xilinx format
   >r dup 0 <# [char] : hold BL hold # # # #               #> r> 2dup + >r swap cmove r>  \ Lattice format
;
: placebyte ( caddr paddr -- caddr+ paddr+ )
   >r dup 1+ swap opcode@ 0 <# # # BL hold #> r> 2dup + >r swap cmove r>
;
: make_romline  ( caddr -- caddr+ saddr slen )
   temp-hex
   here placeaddr  $10 0 DO placebyte LOOP
   #cr over c! 1+ #lf over c! 1+
   here tuck -
;
: MEM-file  ( <name> -- )
   BL word dup c@ 0= abort" file name required"
   count R/W BIN create-file abort" File could not be created" >r
   0 BEGIN  make_romline r@ write-file abort" File write error"
            dup there u>
     UNTIL  drop r> close-file abort" File close failed"
;
