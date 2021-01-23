\ 
\ Last change: KS 29.07.2020 16:25:44
\ Last check in : $Rev: 597 $ $Date:: 2020-11-29 #$
\
\ microCore load screen for simulation.
\ It produces program.mem for initialization of the program memory during simulation.
\
Only Forth also definitions 

[IFDEF] unpatch     unpatch    [ENDIF]
[IFDEF] close-port  close-port [ENDIF]
[IFDEF] microcore   microcore  [ENDIF]   Marker microcore

include extensions.fs           \ Some System word (re)definitions for a more sympathetic environment
include ../vhdl/architecture_pkg_sim.vhd
include microcross.fs           \ the cross-compiler

Target new                      \ go into target compilation mode and initialize target compiler

8 trap-addr code-origin
          0 data-origin

include constants.fs            \ microCore Register addresses and bits
library forth_lib.fs

\ ----------------------------------------------------------------------
\ Booting and TRAPs
\ ----------------------------------------------------------------------

: boot  ( -- )
   r@ 0 l!   55 20 l!   11 -80 l!
   BEGIN REPEAT
;

#reset TRAP: rst    ( -- )            boot              ;  \ compile branch to boot at reset vector location
#isr   TRAP: isr    ( -- )            di IRET           ;
#psr   TRAP: psr    ( -- )                              ;  \ reexecute the previous instruction

end

MEM-file program.mem cr .( sim.fs written to program.mem )
