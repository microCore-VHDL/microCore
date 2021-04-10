\ 
\ Last change: KS 06.03.2021 11:47:05
\
\ MicroCore load screen to simulate the umbilical upload function.
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
   &10  BEGIN  ld swap $101 = UNTIL  1+
        BEGIN  ld swap $102 = UNTIL  1+
        BEGIN  ld swap $103 = UNTIL  1+
        BEGIN  ld swap $104 = UNTIL  drop
[ H data_addr_width cache_addr_width u> T ] [IF]
   #extern BEGIN  ld swap $105 = UNTIL  1+
           BEGIN  ld swap $106 = UNTIL  1+
           BEGIN  ld swap $107 = UNTIL  1+
           BEGIN  ld swap $108 = UNTIL  drop
[THEN]
   #c-bitout Ctrl !
   BEGIN REPEAT
;

#reset TRAP: rst    ( -- )            boot              ;  \ compile branch to boot at reset vector location
#isr   TRAP: isr    ( -- )            di IRET           ;
#psr   TRAP: psr    ( -- )                              ;  \ reexecute the previous instruction

end

MEM-file program.mem cr .( sim_upload.fs written to program.mem )
