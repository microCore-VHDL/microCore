\ 
\ Last change: KS 02.10.2022 18:31:25
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
   $2211 1 cells $20 FOR  over swap st cell+  NEXT  !
   [ H data_addr_width cache_addr_width u> T ] [IF]
      $8877 $6655 #extern st cell+ !
   [THEN]
   host@
   #c-bitout Ctrl !
   BEGIN REPEAT
;

#reset TRAP: rst    ( -- )            boot              ;  \ compile branch to boot at reset vector location
#isr   TRAP: isr    ( -- )            di IRET           ;
#psr   TRAP: psr    ( -- )            pause             ;  \ reexecute the previous instruction

end

MEM-file program.mem cr .( sim_download.fs written to program.mem )
