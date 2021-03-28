\ 
\ Last change: KS 21.03.2021 18:57:11
\ Last check in : $Rev: 675 $ $Date:: 2021-03-25 #$
\
\ MicroCore load screen to simulate program loading via the umbilical.
\
\ Constant progload has to be set to '1' in bench.vhd.
\ CONSTANT MEM_file has to be set to "" in architecture_pkg_sim.vhd
\ Use wave signal script progload.do in the simulator directory.
\
Only Forth also definitions 

[IFDEF] unpatch     unpatch    [ENDIF]
[IFDEF] close-port  close-port [ENDIF]
[IFDEF] microcore   microcore  [ENDIF]   Marker microcore

include extensions.fs           \ Some System word (re)definitions for a more sympathetic environment
include ../vhdl/architecture_pkg_sim.vhd
include microcross.fs           \ the cross-compiler

Target new                      \ go into target compilation mode and initialize target compiler

0 code-origin
0 data-origin

include constants.fs            \ microCore Register addresses and bits

] $5555 4  BEGIN  st ld   REPEAT [

end

VHDL-file ../vhdl/program.vhd cr .( sim_progload.fs written to ../vhdl/program.vhd )
