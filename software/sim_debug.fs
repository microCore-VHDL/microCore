\ 
\ Last change: KS 02.10.2022 16:34:23
\
\ MicroCore load screen for simulating the debug umbilical.
\ It produces program.mem for initialization of the program memory during simulation.
\ Constant debug has to be set to '1' in bench.vhd.
\ Use wave signal script debug.do in the simulator directory.
\
Only Forth also definitions 

[IFDEF] unpatch     unpatch    [ENDIF]
[IFDEF] close-port  close-port [ENDIF]
[IFDEF] microcore   microcore  [ENDIF]   Marker microcore

include extensions.fs           \ Some System word (re)definitions for a more sympathetic environment
include ../vhdl/architecture_pkg_sim.vhd
include microcross.fs           \ the cross-compiler

Target new                      \ go into target compilation mode and initialize target compiler

3 trap-addr code-origin
          0 data-origin

include constants.fs            \ microCore Register addresses and bits

: boot ( -- )
   $6699 $1155 1 st cell+ !
   Debug-reg ld st ld over swap !
   BEGIN REPEAT
;

#reset TRAP: rst    ( -- )            boot              ;  \ compile branch to boot at reset vector location
#isr   TRAP: isr    ( -- )            di IRET           ;
#psr   TRAP: psr    ( -- )            pause             ;  \ reexecute the previous instruction

end

MEM-file program.mem cr .( sim_debug.fs written to program.mem )
