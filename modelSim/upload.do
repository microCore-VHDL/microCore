onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /bench/myFPGA/reset
add wave -noupdate /bench/myFPGA/uCore/debug_unit/deb_reset
add wave -noupdate /bench/myFPGA/uCore/uBus
add wave -noupdate /bench/myFPGA/clk
add wave -noupdate /bench/myFPGA/uCore/clk_en
add wave -noupdate /bench/myFPGA/uCore/uCntrl/r
add wave -noupdate /bench/bitout
add wave -noupdate -divider Debugger
add wave -noupdate /bench/myFPGA/uCore/debug_unit/rxd
add wave -noupdate /bench/myFPGA/uCore/debug_unit/rx_data
add wave -noupdate /bench/myFPGA/uCore/debug_unit/rx_full
add wave -noupdate /bench/myFPGA/uCore/debug_unit/upload
add wave -noupdate /bench/myFPGA/uCore/debug_unit/in_reg
add wave -noupdate /bench/myFPGA/uCore/debug_unit/in_ctr
add wave -noupdate /bench/myFPGA/uCore/debug_unit/in_state
add wave -noupdate /bench/myFPGA/uCore/debug_unit/out_state
add wave -noupdate /bench/myFPGA/uCore/debug_unit/send_ack
add wave -noupdate /bench/myFPGA/uCore/debug_unit/txd
add wave -noupdate /bench/myFPGA/uCore/debug_unit/addr_ptr
add wave -noupdate /bench/myFPGA/uCore/debug_unit/addr_ctr
add wave -noupdate /bench/myFPGA/uCore/debug_unit/deb_drequest
add wave -noupdate /bench/myFPGA/uCore/debug_unit/deb_denable
add wave -noupdate /bench/myFPGA/uCore/memory.enable
add wave -noupdate /bench/myFPGA/uCore/memory.write
add wave -noupdate /bench/myFPGA/uCore/memory.addr
add wave -noupdate /bench/myFPGA/uCore/memory.wdata
add wave -noupdate /bench/myFPGA/uCore/mem_rdata
add wave -noupdate -divider bench
add wave -noupdate /bench/out_buf
add wave -noupdate /bench/sending
add wave -noupdate /bench/uploading
add wave -noupdate /bench/host_ack
add wave -noupdate /bench/host_full
add wave -noupdate /bench/myFPGA/uCore/uBus.mem_en
add wave -noupdate /bench/myFPGA/internal_data_mem/make_sim_mem/internal_data_mem/ram(10)
add wave -noupdate /bench/myFPGA/internal_data_mem/make_sim_mem/internal_data_mem/ram(11)
add wave -noupdate /bench/myFPGA/internal_data_mem/make_sim_mem/internal_data_mem/ram(12)
add wave -noupdate /bench/myFPGA/internal_data_mem/make_sim_mem/internal_data_mem/ram(13)
add wave -noupdate /bench/myFPGA/uCore/uBus.ext_en
add wave -noupdate /bench/ext_SRAM/ram(0)
add wave -noupdate /bench/ext_SRAM/ram(4)
add wave -noupdate /bench/ext_SRAM/ram(8)
add wave -noupdate /bench/ext_SRAM/ram(12)
add wave -noupdate -divider Sequencer
add wave -noupdate /bench/myFPGA/uCore/uCntrl/pause
add wave -noupdate /bench/myFPGA/uCore/uCntrl/r.chain
add wave -noupdate /bench/myFPGA/uCore/uCntrl/instruction
add wave -noupdate /bench/myFPGA/uCore/prog_rdata
add wave -noupdate /bench/myFPGA/uCore/uCntrl/prog_addr
add wave -noupdate /bench/myFPGA/uCore/uCntrl/paddr
add wave -noupdate /bench/myFPGA/uCore/uCntrl/r.pc
add wave -noupdate -divider Datenpfad
add wave -noupdate /bench/myFPGA/uCore/uCntrl/r.tos
add wave -noupdate /bench/myFPGA/uCore/uCntrl/r.nos
add wave -noupdate /bench/myFPGA/uCore/uCntrl/ds_rdata
add wave -noupdate /bench/myFPGA/uCore/uCntrl/r.dsp
add wave -noupdate -divider {Return Stack}
add wave -noupdate /bench/myFPGA/uCore/uCntrl/r.tor
add wave -noupdate /bench/myFPGA/uCore/uCntrl/r.rsp
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {203200 ns} 0} {{Cursor 2} {98200 ns} 0}
quietly wave cursor active 1
configure wave -namecolwidth 154
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
WaveRestoreZoom {0 ns} {273 us}
