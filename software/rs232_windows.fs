\ ----------------------------------------------------------------------
\ @file : rs232_windows.fs
\ ----------------------------------------------------------------------
\
\ Last change: KS 14.06.2020 17:06:46
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
\          cygwin interface under windows.
\          This rs232-interface is unreliable and blocks infrequently
\          due to Microsoft's infamous device drivers.
\
\ Version Author   Date       Changes
\          uho   15-Feb-2009  initial version
\           ks   10-Apr-2020  some code simplifications. Didn't help thou.
\ ----------------------------------------------------------------------
Only Forth also definitions

require lib.fs

library kernel32 kernel32

kernel32 GetCommState    int ptr                     (int) GetCommState    ( handle addr -- r )
kernel32 SetCommState    int ptr                     (int) SetCommState    ( handle addr -- r )
kernel32 CreateFile      ptr int int ptr int int ptr (int) CreateFileA     ( name access share security disp attr temp -- handle )
kernel32 WriteFile       int ptr int ptr ptr         (int) WriteFile       ( handle data size &len &data -- flag )
kernel32 ReadFile        int ptr int ptr ptr         (int) ReadFile        ( handle data size &len &data -- flag )
kernel32 SetCommTimeouts int ptr                     (int) SetCommTimeouts ( handle addr -- flag )
kernel32 GetCommTimeouts int ptr                     (int) GetCommTimeouts ( handle addr -- flag )
kernel32 CloseHandle     int                         (int) CloseHandle     ( handle -- flag )
kernel32 SetupComm       int int int                 (int) SetupComm       ( handle #inq #outq -- flag )

4 4 2Constant int%
2 2 2Constant word%
	    
struct
   int% field DCBlength
   int% field BaudRate
   int% field flags
  word% field wReserved
  word% field XonLim
  word% field XoffLim
  char% field ByteSize
  char% field Parity
  char% field StopBits
  char% field XonChar
  char% field XoffChar
  char% field ErrorChar
  char% field EofChar
  char% field EvtChar
  word% field wReserved1
end-struct DCB

struct
  int% field ReadIntervalTimeout
  int% field ReadTotalTimeoutMultiplier
  int% field ReadTotalTimeoutConstant
  int% field WriteTotalTimeoutMultiplier
  int% field WriteTotalTimeoutConstant
end-struct COMMTIMEOUTS

Create t_buf     DCB %allot drop
Create timeouts  COMMTIMEOUTS %allot drop
&128 Constant #buf-size

0 Value handle

$80000000 Constant GENERIC_READ
$40000000 Constant GENERIC_WRITE
        3 Constant OPEN_EXISTING
	    
: open-port ( addr u -- )
   tuck pad swap move   0 swap pad + c!   \ 0 terminated string
   pad GENERIC_READ GENERIC_WRITE or 0 0 OPEN_EXISTING 0 0 CreateFile
   dup -1 = abort" serial port not available"   TO handle
   handle #buf-size dup SetupComm drop
;
: close-port ( -- )
   handle ?dup 0= ?EXIT
   CloseHandle 0= abort" can not close serial port"
   0 TO handle
;
 &19200 Constant B19200
 &38400 Constant B38400
 &57600 Constant B57600
&115200 Constant B115200

: set-baud ( baud handle -- )  >r
   r@ timeouts GetCommTimeouts drop
   -1 timeouts ReadIntervalTimeout !
    0 timeouts ReadTotalTimeoutMultiplier !
    0 timeouts ReadTotalTimeoutConstant !
    0 timeouts WriteTotalTimeoutMultiplier !
    0 timeouts WriteTotalTimeoutConstant !
   r@ timeouts SetCommTimeouts drop
   t_buf DCB %size erase
   DCB %size t_buf DCBlength !
   $1011 t_buf flags !
   t_buf BaudRate !
   8 t_buf ByteSize c!
   2 t_buf StopBits c!          \ 2 stop bits
   0 t_buf Parity c!            \ no parity
   r> t_buf SetCommState drop
;
: reset-baud ( handle -- )  drop ;

Create read-buf #buf-size 1+ chars allot  \ 0-terminated string, therfore 1+
Variable read-len

: term-read ( -- addr u )
  handle read-buf #buf-size read-len 0 ReadFile drop
  read-buf read-len @
;
: term-key? ( -- flag )
   read-len @ IF  true EXIT THEN
   term-read   dup read-len !  nip 0<>
;
: term-key  ( -- char )
   BEGIN  term-key? 0= WHILE  $80000 FOR NEXT  REPEAT    \ some waiting on 0, it does not work reliably otherwise
   read-buf c@   -1 read-len +!
   read-buf char+ read-buf read-len @ cmove
;
Variable write-buf
Variable write-len

: (term-type) ( addr u -- )  handle -rot write-len 0 WriteFile drop ;

: term-emit   ( char -- )    write-buf c!  write-buf 1 (term-type) ;
   
: term-flush  ( -- ) ;

: Umbilical:  ( baud <name> -- )  \ e.g. B115200 Umbilical: COM3
   name open-port   handle set-baud
;
