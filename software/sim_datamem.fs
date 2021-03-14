\ 
\ Last change: KS 15.11.2020 14:03:48
\ Last check in : $Rev: 589 $ $Date:: 2020-11-19 #$
\
\ MicroCore load screen for simulating internal and external data memory.
\ It produces program.mem for initialization of the program memory during simulation.
\ Use wave signal scripts datamem.do and ext_datamem.do in the simulator directory.
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

data_width &19 < [IF]
     $1234 Constant #data  \ 16 and 18 bit
[ELSE]
   $123456 Constant #data  \ >= 24 bits
[THEN]

: datatest  ( -- )
   #data    0 st ld 1+ st @
         #extern st ld 1+ st @
;
: boot  ( -- )
   datatest
   BEGIN  REPEAT
;

#reset TRAP: rst    ( -- )            boot              ;  \ compile branch to boot at reset vector location
#isr   TRAP: isr    ( -- )            di IRET           ;
#psr   TRAP: psr    ( -- )                              ;  \ reexecute the previous instruction
#does> TRAP: dodoes ( addr -- addr' ) ld 1+ swap BRANCH ;  \ the DOES> runtime primitive
#data! TRAP: data!  ( dp n -- dp+1 )  swap st 1+        ;  \ Data memory initialization

end

MEM-file program.mem cr .( sim_datamem.fs written to program.mem )
