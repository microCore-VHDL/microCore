\ 
\ Last change: KS 06.03.2021 16:44:11
\ Last check in : $Rev: 584 $ $Date:: 2020-11-11 #$
\
\ MicroCore load screen to simulate the umbilical download function.
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
   $3344 $1122    1 st 1+ !
[ H data_addr_width cache_addr_width u> T ] [IF]
   $7788 $5566 #extern st 1+ !
[THEN]
   BEGIN  Debug-reg @ drop  REPEAT
;

#reset TRAP: rst    ( -- )            boot              ;  \ compile branch to boot at reset vector location
#isr   TRAP: isr    ( -- )            di IRET           ;
#psr   TRAP: psr    ( -- )                              ;  \ reexecute the previous instruction

end

MEM-file program.mem cr .( sim_download.fs written to program.mem )
