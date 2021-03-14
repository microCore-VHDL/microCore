\ 
\ Last change: KS 05.03.2021 17:41:28
\ Last check in : $Rev: 656 $ $Date:: 2021-03-06 #$
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
   $6699 $1155 1 st 1+ !
   Debug-reg ld st ld over swap !
   $4002 = IF  #c-bitout Ctrl !  THEN
   BEGIN REPEAT
;

#reset TRAP: rst    ( -- )            boot              ;  \ compile branch to boot at reset vector location
#isr   TRAP: isr    ( -- )            di IRET           ;
#psr   TRAP: psr    ( -- )                              ;  \ reexecute the previous instruction

end

MEM-file program.mem cr .( sim_debug.fs written to program.mem )
