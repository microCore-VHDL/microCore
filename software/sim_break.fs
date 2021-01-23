\ 
\ Last change: KS 20.08.2020 15:31:39
\ Last check in : $Rev: 584 $ $Date:: 2020-11-11 #$
\
\ MicroCore load screen for simulating the umbilical's break function.
\ Constant break has to be set to '1' in bench.vhd.
\ Use wave signal script break.do in the simulator directory.
\
Only Forth also definitions 

[IFDEF] unpatch     unpatch    [ENDIF]
[IFDEF] close-port  close-port [ENDIF]
[IFDEF] microcore   microcore  [ENDIF]   Marker microcore

include extensions.fs           \ Some System word (re)definitions for a more sympathetic environment
include ../vhdl/architecture_pkg_sim.vhd
include microcross.fs           \ the cross-compiler

Target new initialized          \ go into target compilation mode and initialize target compiler

8 trap-addr code-origin
          0 data-origin
      
include constants.fs            \ microCore Register addresses and bits
include debugger.fs
library forth_lib.fs
library task_lib.fs

Task Background

Variable Counter

: bg_task  ( -- )   0 Counter !  BEGIN  pause  1 Counter +!  REPEAT ;

: boot  ( -- )  
   0 task>dsp Dsp !   0 task>rsp Rsp !   
   Terminal Background ['] bg_task spawn
   debugService
;

#reset TRAP: rst   ( -- ) boot           ;  \ compile branch to TEST at reset vector location
#isr   TRAP: isr   ( -- ) di IRET        ;
#psr   TRAP: psr   ( -- ) pause          ;  \ reexecute the previous instruction
#break TRAP: break ( -- ) debugger       ;

end

MEM-file program.mem cr .( sim_break.fs written to program.mem )
