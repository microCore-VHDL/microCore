\ 
\ Last change: KS 02.10.2022 16:32:02
\
\ MicroCore load screen for simulating the host <-> target synchronization.
\ Constant handshake has to be set to '1' in bench.vhd.
\ Use wave signal script handshake.do in the simulator directory.
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

Variable extern

: boot  ( -- )  \ handshake protocol from monitor.fs
   0 extern !   #i-ext int-enable   ei
   host@ drop
   BEGIN  #warmboot host!   host@ $3F5 = UNTIL
   BEGIN  $305 host!   host@ 0= UNTIL     \ synchronise Host <-> Target communication
   BEGIN  0 host!  host@ execute  REPEAT  \ the "monitor" loop
;
: interrupt ( -- )   1 extern +! ;

#reset TRAP: rst    ( -- )            boot              ;  \ compile branch to boot at reset vector location
#isr   TRAP: isr    ( -- )            interrupt IRET    ;
#psr   TRAP: psr    ( -- )            pause             ;  \ reexecute the previous instruction

end

MEM-file program.mem cr .( sim_handshake.fs written to program.mem )
