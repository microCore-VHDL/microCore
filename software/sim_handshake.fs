\ 
\ Last change: KS 20.06.2020 16:25:49
\ Last check in : $Rev: 584 $ $Date:: 2020-11-11 #$
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
   host> drop
   BEGIN  #warmboot >host   host> $5F5 = UNTIL
   BEGIN  $505 >host   host> 0= UNTIL     \ synchronise Host <-> Target communication
   BEGIN  0 >host  host> execute  REPEAT  \ the "monitor" loop
;
: interrupt ( -- )   1 extern +! ;

#reset TRAP: rst    ( -- )            boot              ;  \ compile branch to boot at reset vector location
#isr   TRAP: isr    ( -- )            interrupt IRET    ;
#psr   TRAP: psr    ( -- )                              ;  \ reexecute the previous instruction
#does> TRAP: dodoes ( addr -- addr' ) ld 1+ swap BRANCH ;  \ the DOES> runtime primitive
#data! TRAP: data!  ( dp n -- dp+1 )  swap st 1+        ;  \ Data memory initialization

end

MEM-file program.mem cr .( sim_handshake.fs written to program.mem )
