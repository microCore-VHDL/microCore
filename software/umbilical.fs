\ ----------------------------------------------------------------------
\ @file : umbilical.fs
\ ----------------------------------------------------------------------
\
\ Last change: KS 24.03.2021 17:54:56
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
\ @brief : This is the top level configuration file for the rs232 interface.
\          Depending on your host's OS you have to comment-in the
\          appropriate driver and fill in the name of the actual
\          rs232 interface used.
\
\ Version Author   Date       Changes
\   210     ks   14-Jun-2020  initial version
\ ----------------------------------------------------------------------
SIMULATION [NOTIF]

 include rs232_linux.fs     B115200 Umbilical: /dev/ttyUSB0
\ include rs232_windows.fs   B115200 Umbilical: COM3
\ include rs232_macosx.fs    B115200 Umbilical: /dev/cu.usbserial

[THEN]
