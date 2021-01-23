\ 
\ Last change: KS 16.05.2020 18:25:36
\ Last check in : $Rev: 584 $ $Date:: 2020-11-11 #$
\
\ MicroCore load screen to simulate the umbilical up- and download function.
\ Either constant download or upload has to be set to '1' in bench.vhd.
\ Use wave signal script download.do and upload.do in the simulator directory.
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
   $3344 $1122 1 st 1+ !
\   $12345 $67890 $1000 st 1+ !
\   $5555 4 >r BEGIN  r> st noop ld >r 1+  REPEAT
    BEGIN  Debug-reg @ drop  REPEAT
;

#reset TRAP: rst    ( -- )            boot              ;  \ compile branch to boot at reset vector location
#isr   TRAP: isr    ( -- )            di IRET           ;
#psr   TRAP: psr    ( -- )                              ;  \ reexecute the previous instruction

end

MEM-file program.mem cr .( sim_updown.fs written to program.mem )   
