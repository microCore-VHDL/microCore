onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /bench/myFPGA/reset
add wave -noupdate /bench/myFPGA/clk
add wave -noupdate /bench/myFPGA/uCore/cycle_ctr
add wave -noupdate /bench/myFPGA/uCore/clk_en
add wave -noupdate /bench/myFPGA/uCore/uBus
add wave -noupdate /bench/myFPGA/uCore/uCntrl/s
add wave -noupdate /bench/myFPGA/flags
add wave -noupdate /bench/myFPGA/ext_memory
add wave -noupdate -divider {address diffs}
add wave -noupdate /architecture_pkg/data_width
add wave -noupdate /architecture_pkg/chunks
add wave -noupdate /architecture_pkg/subbits
add wave -noupdate /bench/myFPGA/with_ext_mem/SRAM/leader
add wave -noupdate -radix unsigned /bench/myFPGA/with_ext_mem/SRAM/residue
add wave -noupdate -divider SRAM
add wave -noupdate /bench/myFPGA/with_ext_mem/SRAM/delay_ctr
add wave -noupdate /bench/myFPGA/ext_memory.enable
add wave -noupdate /bench/ext_ce_n
add wave -noupdate /bench/myFPGA/ext_memory.addr
add wave -noupdate /bench/ext_we_n
add wave -noupdate /bench/ext_oe_n
add wave -noupdate -radix hexadecimal -childformat {{/bench/ext_addr(13) -radix hexadecimal} {/bench/ext_addr(12) -radix hexadecimal} {/bench/ext_addr(11) -radix unsigned} {/bench/ext_addr(10) -radix unsigned} {/bench/ext_addr(9) -radix unsigned} {/bench/ext_addr(8) -radix unsigned} {/bench/ext_addr(7) -radix unsigned} {/bench/ext_addr(6) -radix unsigned} {/bench/ext_addr(5) -radix unsigned} {/bench/ext_addr(4) -radix unsigned} {/bench/ext_addr(3) -radix unsigned} {/bench/ext_addr(2) -radix unsigned} {/bench/ext_addr(1) -radix unsigned} {/bench/ext_addr(0) -radix unsigned}} -subitemconfig {/bench/ext_addr(13) {-height 15 -radix hexadecimal} /bench/ext_addr(12) {-height 15 -radix hexadecimal} /bench/ext_addr(11) {-height 15 -radix unsigned} /bench/ext_addr(10) {-height 15 -radix unsigned} /bench/ext_addr(9) {-height 15 -radix unsigned} /bench/ext_addr(8) {-height 15 -radix unsigned} /bench/ext_addr(7) {-height 15 -radix unsigned} /bench/ext_addr(6) {-height 15 -radix unsigned} /bench/ext_addr(5) {-height 15 -radix unsigned} /bench/ext_addr(4) {-height 15 -radix unsigned} /bench/ext_addr(3) {-height 15 -radix unsigned} /bench/ext_addr(2) {-height 15 -radix unsigned} /bench/ext_addr(1) {-height 15 -radix unsigned} /bench/ext_addr(0) {-height 15 -radix unsigned}} /bench/ext_addr
add wave -noupdate /bench/ext_data
add wave -noupdate /bench/ext_SRAM/initialized_ram/ram(0)
add wave -noupdate /bench/ext_SRAM/initialized_ram/ram(1)
add wave -noupdate /bench/ext_SRAM/initialized_ram/ram(2)
add wave -noupdate /bench/ext_SRAM/initialized_ram/ram(3)
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
add wave -noupdate /bench/myFPGA/uCore/mem_addr
add wave -noupdate /bench/myFPGA/uCore/uCntrl/r.rsp
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {2381 ns} 0} {{Cursor 2} {2211 ns} 0}
quietly wave cursor active 2
configure wave -namecolwidth 145
configure wave -valuecolwidth 69
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
WaveRestoreZoom {0 ns} {9450 ns}
