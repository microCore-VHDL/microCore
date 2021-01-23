\ ----------------------------------------------------------------------
\ @file : rs232_macosx.fs
\ ----------------------------------------------------------------------
\
\ Last change: KS 14.06.2020 17:06:42
\ Project : microCore
\ Language : gforth_0.6.2
\ Last check in : $Rev: 572 $ $Date:: 2020-06-24 #$
\ @copyright (c): Free Software Foundation
\ @original author: uho - Ulrich Hoffmann
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
\ @brief : Terminal for gforth/R8C hacked by uho to run as an rs232
\          interface under MAC-OS.
\
\ Version Author   Date       Changes
\          uho   15-Feb-2009  initial version
\ ----------------------------------------------------------------------
Only Forth also definitions

require lib.fs

[IFUNDEF] libc  library libc libc.dylib  [THEN]

libc tcgetattr int ptr     (int) tcgetattr ( port termios -- r )
libc tcsetattr int int ptr (int) tcsetattr ( port opt termios -- r )
libc tcflow    int int     (int) tcflow    ( port action -- r )
libc ioctl<p>  int int ptr (int) ioctl     ( d request ptr -- r )
libc fileno    ptr         (int) fileno    ( handle -- port )

cell dup 2Constant int%
       20 Constant NCCS

struct
    int% field c_iflag
    int% field c_oflag
    int% field c_cflag
    int% field c_lflag
    char% NCCS * field c_line
    int% field c_ispeed
    int% field c_ospeed
end-struct termios

Create t_old  termios %allot drop
Create t_buf  termios %allot drop

     $300 Constant CS8
     $800 Constant CREAD
    $8000 Constant CLOCAL
        0 Constant CBAUD
        1 Constant IGNBRK
        4 Constant IGNPAR
      &17 Constant VTIME
      &16 Constant VMIN
$4004667F Constant FIONREAD

   &19200 Constant B19200
   &38400 Constant B38400
   &57600 Constant B57600
  &115200 Constant B115200
  
: set-baud ( baud handle -- )
   fileno >r r@ t_old tcgetattr drop
   t_old t_buf termios %size move
   [ IGNBRK IGNPAR or       ] Literal t_buf c_iflag !
   0                                  t_buf c_oflag !
   [ CS8 CREAD or CLOCAL or ] Literal t_buf c_cflag !
   0                                  t_buf c_lflag !
   dup t_buf c_ispeed !
       t_buf c_ospeed !
   0 t_buf c_line VMIN + c!
   0 t_buf c_line VTIME + c!
   r> 1 t_buf tcsetattr drop
;
: reset-baud ( handle -- )   fileno 1 t_old tcsetattr drop ;

: check-read ( handle -- n ) fileno >r 0 sp@ r> FIONREAD rot ioctl<p> drop ;

0 Value handle

: open-port   ( addr u -- ) r/w open-file throw   TO handle ;

: term-read   ( -- addr u ) pad handle check-read handle read-file throw pad swap ;

: term-emit   ( char -- )   handle emit-file throw ;

: (term-type) ( addr u -- ) handle write-file throw ;

: term-flush  ( -- )        handle flush-file throw ;
   
Create read-buf $400 chars allot
Variable read-len  0 read-len !
            
: term-key? ( -- flag ) 
    read-len @ IF  true EXIT THEN
    term-read dup read-len !  read-buf swap chars cmove 
    read-len @ 0<> ; 
 
: term-key ( -- char )
   BEGIN  term-key? UNTIL
   read-buf c@   -1 read-len +!
   read-buf char+ read-buf read-len @ cmove
;
: Umbilical: ( baud <name> -- )  \ e.g. B115200 Umbilical: /dev/cu.usbserial
   name open-port   handle set-baud
;
