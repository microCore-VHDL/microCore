\ ----------------------------------------------------------------------
\ @file : rs232_linux.fs
\ ----------------------------------------------------------------------
\
\ Last change: KS 27.03.2021 22:14:28
\ Last check in: $Rev: 674 $ $Date:: 2021-03-24 #$
\ @project: microCore
\ @language: gforth_0.6.2
\ @copyright (c): Free Software Foundation
\ @original author: uho - Ulrich Hoffmann
\ @contributor: ks - Klaus Schleisiek
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
\          interface under linux.
\
\ Version Author   Date       Changes
\          uho   15-Feb-2009  initial version
\           ks   10-Apr-2020  addition of the gforth_07x version.
\ ----------------------------------------------------------------------
Only Forth also definitions

gforth_062 [IF]   cr .( UART for gforth_062, 32 bit)

require lib.fs

[IFUNDEF] libc  library libc libc.so.6  [THEN]

libc tcgetattr int ptr     (int) tcgetattr ( port termios -- r )
libc tcsetattr int int ptr (int) tcsetattr ( port opt termios -- r )
libc tcflow    int int     (int) tcflow    ( port action -- r )
libc ioctl<p>  int int ptr (int) ioctl     ( d request ptr -- r )
libc fileno    ptr         (int) fileno    ( file* -- port )

4 4 2Constant int%
$20 Constant NCCS

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

    1 Constant B50
    2 Constant B75
    3 Constant B110
    4 Constant B134
    5 Constant B150
    6 Constant B200
    7 Constant B300
    8 Constant B600
    9 Constant B1200
   $A Constant B1800
   $B Constant B2400
   $C Constant B4800
   $D Constant B9600
   $E Constant B19200
   $F Constant B38400
$1001 Constant B57600
$1002 Constant B115200
  $30 Constant CS8      \ 8 bits/character
  $80 Constant CREAD    \ input enable
 $400 Constant CSTOPB   \ 2 stop bits
 $800 Constant CLOCAL   \ don't check RS232 status lines
    1 Constant IGNBRK   \ ignore break
    4 Constant IGNPAR   \ ignore parity/framing errors
    6 Constant VTIME
    7 Constant VMIN

: set-baud ( baud handle -- )
   fileno >r   r@ t_old tcgetattr drop
   t_old t_buf termios %size move
   [ IGNBRK IGNPAR or                 ] Literal    t_buf c_iflag !
   0                                               t_buf c_oflag !
   [ CS8 CSTOPB or CREAD or CLOCAL or ] Literal or t_buf c_cflag !
   0                                               t_buf c_lflag !
   0 t_buf c_line 2dup VMIN + c!   VTIME + c!      \ return after every read, even if no char available
   &28800 t_buf c_cflag @ $F and lshift
   dup t_buf c_ispeed !   t_buf c_ospeed !
   r> 1 t_buf tcsetattr drop
;
: reset-baud ( handle -- )     fileno 1 t_old tcsetattr drop ;

[THEN]  gforth_079 gforth_072 or [IF]   cr .( UART for gforth_07x, 32/64 bit)

require libcc.fs

c-library serial
    \c #include <termios.h>
    \c #include <sys/ioctl.h>
    \c #include <sys/types.h>
    \c #include <sys/stat.h>
    \c #include <stdio.h>
    \c #include <unistd.h>
    \c #include <fcntl.h>

    c-function tcgetattr   tcgetattr   n a -- n     ( port termios -- r )
    c-function tcsetattr   tcsetattr   n n a -- n   ( port opt termios -- r )
    c-function cfmakeraw   cfmakeraw   a -- void    ( termios -- )
    c-function cfsetispeed cfsetispeed a n -- n     ( termios speed -- r )
    c-function cfsetospeed cfsetospeed a n -- n     ( termios speed -- r )
    c-function tcflow      tcflow      n n -- n     ( port action -- n )
    c-function ioctl       ioctl       n n a -- n   ( port cmd ptr -- n )
    c-function setvbuf     setvbuf     a a n n -- n ( file* buf mode size -- r )
end-c-library

[IFDEF] android
    ' wfield: alias flagfield: ( offset -- offset' )
    ' w@ alias flag@
    ' w! alias flag!
[ELSE]
    ' lfield: alias flagfield: ( offset -- offset' )
    ' l@ alias flag@
    ' l! alias flag!
[THEN]

begin-structure termios
flagfield: c_iflag           \ input mode flags
flagfield: c_oflag           \ output mode flags
flagfield: c_cflag           \ control mode flags
flagfield: c_lflag           \ local mode flags
cfield: c_line
   32 +field c_cc            \ line discipline
flagfield: c_ispeed          \ input speed
flagfield: c_ospeed          \ output speed
end-structure

Create t_old  termios allot
Create t_buf  termios allot

    1 Constant B50
    2 Constant B75
    3 Constant B110
    4 Constant B134
    5 Constant B150
    6 Constant B200
    7 Constant B300
    8 Constant B600
    9 Constant B1200
   $A Constant B1800
   $B Constant B2400
   $C Constant B4800
   $D Constant B9600
   $E Constant B19200
   $F Constant B38400
$1001 Constant B57600
$1002 Constant B115200
$1003 Constant B230400
$1004 Constant B460800
$1005 Constant B500000
$1006 Constant B576000
$1007 Constant B921600
$1008 Constant B1000000
$1009 Constant B1152000
$100A Constant B1500000
$100B Constant B2000000
$100C Constant B2500000
$100D Constant B3000000
$100E Constant B3500000
$100F Constant B4000000
  $30 Constant CS8
  $80 Constant CREAD
  $40 Constant CSTOPB
 $800 Constant CLOCAL
 $800 Constant IXANY
    1 Constant IGNBRK
    4 Constant IGNPAR
 $100 Constant NOCTTY
 $800 Constant NODELAY
    5 Constant VTIME
    6 Constant VMIN

: set-baud ( baud handle -- )
   fileno >r    r@ t_old tcgetattr ?ior
   t_old t_buf termios move
   t_buf cfmakeraw
   t_buf c_iflag dup flag@ CLOCAL or swap flag!
   t_buf over cfsetispeed ?ior
   t_buf swap cfsetospeed ?ior
   0 t_buf c_line 2dup VMIN + c!   VTIME + c!      \ return after every read, even if no char available
   r> 0 t_buf tcsetattr ?ior
;
: reset-baud ( handle -- )  fileno 0 t_old tcsetattr ?ior ;

[THEN]

0 Value handle

: open-port ( addr u -- )  r/w open-file throw  to handle ;

: close-port ( -- )        handle ?dup 0= ?EXIT   close-file throw   0 TO handle ;

&120 Constant #buf-length
Create read-buf #buf-length chars allot

: term-read   ( -- u )       read-buf #buf-length handle read-file throw ;

: term-flush  ( -- )         handle flush-file throw ;

: term-emit   ( char -- )    handle  emit-file throw ;

: (term-type) ( addr u -- )  handle write-file throw term-flush ;

Variable read-len  0 read-len !
            
: term-key? ( -- flag )   read-len @ IF true EXIT THEN   term-read   dup read-len !   0<> ;

: term-key ( -- char )
   BEGIN  term-key? UNTIL
   read-buf c@   -1 read-len +!
   read-buf char+ read-buf read-len @ cmove
;
: Umbilical: ( baud <name> -- )    \ e.g. B115200 Umbilical: /dev/ttyUSB0
   name open-port   handle set-baud
;
