onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /bench/myFPGA/uCore/warmboot
add wave -noupdate /bench/myFPGA/reset_n
add wave -noupdate /bench/myFPGA/reset
add wave -noupdate /bench/myFPGA/clk
add wave -noupdate /bench/myFPGA/uCore/cycle_ctr
add wave -noupdate /bench/myFPGA/uCore/clk_en
add wave -noupdate /bench/myFPGA/uCore/uBus
add wave -noupdate /bench/myFPGA/uCore/uCntrl/r
add wave -noupdate /bench/myFPGA/uCore/uCntrl/s
add wave -noupdate /bench/myFPGA/flags
add wave -noupdate /bench/bitout
add wave -noupdate -divider Sequencer
add wave -noupdate /bench/myFPGA/uCore/uCntrl/s.lit
add wave -noupdate /bench/myFPGA/uCore/uCntrl/r.chain
add wave -noupdate /bench/myFPGA/uCore/uCntrl/r.inst
add wave -noupdate /bench/myFPGA/uCore/uCntrl/instruction
add wave -noupdate /bench/myFPGA/uCore/prog_rdata
add wave -noupdate /bench/myFPGA/uCore/uCntrl/prog_addr
add wave -noupdate /bench/myFPGA/uCore/uCntrl/paddr
add wave -noupdate /bench/myFPGA/uCore/uCntrl/r.pc
add wave -noupdate -divider Datenpfad
add wave -noupdate /bench/myFPGA/uCore/uCntrl/r.tos
add wave -noupdate /bench/myFPGA/uCore/uCntrl/r.nos
add wave -noupdate /bench/myFPGA/uCore/uCntrl/ds_rdata
add wave -noupdate /bench/myFPGA/uCore/uCntrl/stack_addr
add wave -noupdate /bench/myFPGA/uCore/uCntrl/r.dsp
add wave -noupdate -divider {Return Stack}
add wave -noupdate /bench/myFPGA/uCore/uCntrl/r.tor
add wave -noupdate /bench/myFPGA/uCore/mem_rdata
add wave -noupdate /bench/myFPGA/uCore/uCntrl/r.rsp
add wave -noupdate /bench/myFPGA/uCore/uCntrl/mem_addr
add wave -noupdate /bench/myFPGA/cache_addr
add wave -noupdate -divider Alu
add wave -noupdate /bench/myFPGA/uCore/uCntrl/cin
add wave -noupdate /bench/myFPGA/uCore/uCntrl/ladd_x
add wave -noupdate /bench/myFPGA/uCore/uCntrl/ladd_y
add wave -noupdate /bench/myFPGA/uCore/uCntrl/ladd_out
add wave -noupdate /bench/myFPGA/uCore/uCntrl/multiplicand
add wave -noupdate /bench/myFPGA/uCore/uCntrl/multiplier
add wave -noupdate /bench/myFPGA/uCore/uCntrl/product
add wave -noupdate -divider Pause
add wave -noupdate /bench/myFPGA/uCore/uCntrl/pause
add wave -noupdate -divider interrupt
add wave -noupdate /bench/myFPGA/flags(0)
add wave -noupdate /bench/myFPGA/uCore/uCntrl/ienable
add wave -noupdate /bench/myFPGA/uCore/uCntrl/s.ie
add wave -noupdate /bench/myFPGA/int_n
add wave -noupdate /bench/myFPGA/uCore/uCntrl/pending
add wave -noupdate /bench/myFPGA/uCore/uCntrl/interrupt
add wave -noupdate /bench/myFPGA/uCore/uCntrl/s.iis
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {2178 ns} 0} {{Cursor 2} {121737 ns} 0}
quietly wave cursor active 1
configure wave -namecolwidth 111
configure wave -valuecolwidth 68
configure wave -justifyvalue left
configure wave -signalnamewidth 1
configure wave -snapdistance 10
configure wave -datasetprefix 0
configure wave -rowmargin 4
configure wave -childrowmargin 2
configure wave -gridoffset 0
configure wave -gridperiod 1
configure wave -griddelta 40
configure wave -timeline 0
configure wave -timelineunits ns
update
WaveRestoreZoom {1694 ns} {3075 ns}
