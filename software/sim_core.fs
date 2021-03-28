\ 
\ Last change: KS 25.03.2021 20:56:31
\ Last check in : $Rev: 677 $ $Date:: 2021-03-27 #$
\
\ MicroCore load screen for coretest simulation.
\ It produces program.mem for initialization of the program memory during simulation.
\ Use wave signal script core.do in the simulator directory.
\
Only Forth also definitions 

[IFDEF] unpatch     unpatch    [ENDIF]
[IFDEF] close-port  close-port [ENDIF]
[IFDEF] microcore   microcore  [ENDIF]   Marker microcore

include extensions.fs           \ Some System word (re)definitions for a more sympathetic environment
include ../vhdl/architecture_pkg_sim.vhd
include microcross.fs           \ the cross-compiler

\ Verbose on

Target new initialized          \ go into target compilation mode and initialize target compiler

9 trap-addr code-origin
          0 data-origin

include constants.fs            \ MicroCore Register addresses and bits
library forth_lib.fs
include coretest.fs

: boot  ( -- )   CALL INITIALIZATION coretest BEGIN REPEAT ;

\ ----------------------------------------------------------------------
\ Booting and TRAPs
\ ----------------------------------------------------------------------

#reset TRAP: rst    ( -- )            boot              ;  \ compile branch to coretest at reset vector location
#isr   TRAP: isr    ( -- )            interrupt IRET    ;
#psr   TRAP: psr    ( -- )            #f-sema release   ;  \ matches coretest's test_sema
#data! TRAP: data!  ( dp n -- dp+1 )  swap st 1+        ;  \ Data memory initialization

end

MEM-file program.mem  cr .( sim_core.fs written to program.mem )
