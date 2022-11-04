\ 
\ Last change: KS 04.10.2022 22:34:44
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

WITH_BYTES [IF]

   : boot ( -- )  8 cells  \ mem-addr on uCore
      BEGIN  cld swap $11 = UNTIL  1+
      BEGIN  cld swap $12 = UNTIL  1+
      BEGIN  cld swap $13 = UNTIL  1+
      BEGIN  cld swap $14 = UNTIL  1+
      BEGIN  cld swap $15 = UNTIL  1+
      BEGIN  cld swap $16 = UNTIL  drop
      [ H data_addr_width cache_addr_width u> T ] [IF]
         #extern
         BEGIN  cld swap $25 = UNTIL  1+
         BEGIN  cld swap $26 = UNTIL  1+
         BEGIN  cld swap $27 = UNTIL  1+
         BEGIN  cld swap $28 = UNTIL  drop
      [THEN]
      #c-bitout Ctrl !
      BEGIN REPEAT
   ;
[ELSE] \ cell addressed

   : boot ( -- )  8 cells  \ mem-addr on uCore
      BEGIN  ld swap $11 = UNTIL  1+
      BEGIN  ld swap $12 = UNTIL  1+
      BEGIN  ld swap $13 = UNTIL  1+
      BEGIN  ld swap $14 = UNTIL  drop
      [ H data_addr_width cache_addr_width u> T ] [IF]
         #extern
         BEGIN  ld swap $25 = UNTIL  1+
         BEGIN  ld swap $26 = UNTIL  1+
         BEGIN  ld swap $27 = UNTIL  1+
         BEGIN  ld swap $28 = UNTIL  drop
      [THEN]
      #c-bitout Ctrl !
      BEGIN REPEAT
   ;
[THEN]

#reset TRAP: rst    ( -- )            boot              ;  \ compile branch to boot at reset vector location
#isr   TRAP: isr    ( -- )            di IRET           ;
#psr   TRAP: psr    ( -- )            pause             ;  \ reexecute the previous instruction

end

MEM-file program.mem cr .( sim_upload.fs written to program.mem )
