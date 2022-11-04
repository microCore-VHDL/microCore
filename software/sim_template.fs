\ 
\ Last change: KS 02.10.2022 16:34:45
\
\ MicroCore load screen for simulation.
\ It produces program.mem for initialization of the program memory during simulation.
\ Use wave signal script xxxxxxxxx.do in the simulator directory.
\
Only Forth also definitions 

[IFDEF] unpatch     unpatch    [ENDIF]
[IFDEF] close-port  close-port [ENDIF]
[IFDEF] microcore   microcore  [ENDIF]   Marker microcore

include extensions.fs           \ Some System word (re)definitions for a more sympathetic environment
include ../vhdl/architecture_pkg_sim.vhd
include microcross.fs           \ the cross-compiler

Target new initialized          \ go into target compilation mode and initialize target compiler

8 trap-addr code-origin
          0 data-origin

include constants.fs            \ microCore Register addresses and bits
library forth_lib.fs

\ ----------------------------------------------------------------------
\ Interrupt
\ ----------------------------------------------------------------------

: interrupt ( -- )  Intflags @ drop ;

\ ----------------------------------------------------------------------
\ Booting and TRAPs
\ ----------------------------------------------------------------------

: boot  ( -- )
   BEGIN REPEAT
;

#reset TRAP: rst    ( -- )            boot                 ;  \ compile branch to boot at reset vector location
#isr   TRAP: isr    ( -- )            interrupt IRET       ;
#psr   TRAP: psr    ( -- )            pause                ;  \ reexecute the previous instruction
#does> TRAP: dodoes ( addr -- addr' ) ld cell+ swap BRANCH ;  \ the DOES> runtime primitive
#data! TRAP: data!  ( dp n -- dp+1 )  swap st cell+        ;  \ Data memory initialization

end

MEM-file program.mem cr .( written to program.mem )   
