onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /bench/myFPGA/uCore/warmboot
add wave -noupdate /bench/myFPGA/reset
add wave -noupdate /bench/myFPGA/uCore/debug_unit/deb_reset
add wave -noupdate /bench/myFPGA/uCore/uBus
add wave -noupdate /bench/myFPGA/clk
add wave -noupdate /bench/myFPGA/uCore/cycle_ctr
add wave -noupdate /bench/myFPGA/uCore/clk_en
add wave -noupdate -divider Debugger
add wave -noupdate /bench/myFPGA/uCore/debug_unit/send_ack
add wave -noupdate /bench/myFPGA/uCore/debug_unit/txd
add wave -noupdate /bench/myFPGA/uCore/debug_unit/rxd
add wave -noupdate /bench/myFPGA/uCore/debug_unit/rx_data
add wave -noupdate /bench/myFPGA/uCore/debug_unit/rx_full
add wave -noupdate /bench/myFPGA/uCore/debug_unit/rx_read
add wave -noupdate /bench/myFPGA/uCore/debug_unit/in_ctr
add wave -noupdate /bench/myFPGA/uCore/debug_unit/in_reg
add wave -noupdate /bench/myFPGA/uCore/debug_unit/in_state
add wave -noupdate /bench/myFPGA/uCore/debug_unit/deb_prequest
add wave -noupdate /bench/myFPGA/uCore/deb_penable
add wave -noupdate /bench/myFPGA/uCore/debug_unit/upload
add wave -noupdate /bench/myFPGA/uCore/debug_unit/out_state
add wave -noupdate /bench/myFPGA/uCore/debug_unit/addr_ptr
add wave -noupdate /bench/myFPGA/uCore/debug_unit/addr_ctr
add wave -noupdate /bench/myFPGA/uCore/debug_unit/write
add wave -noupdate /bench/myFPGA/uCore/debug_unit/debugmem.enable
add wave -noupdate /bench/myFPGA/uCore/debug_unit/debugmem.write
add wave -noupdate /bench/myFPGA/uCore/debug_unit/debugmem.addr
add wave -noupdate /bench/myFPGA/uCore/debug_unit/debugmem.wdata
add wave -noupdate /bench/myFPGA/uCore/memory.wdata
add wave -noupdate -divider progmem
add wave -noupdate /bench/myFPGA/uCore/internal_prog_mem/paddr
add wave -noupdate /bench/myFPGA/uCore/internal_prog_mem/penable
add wave -noupdate /bench/myFPGA/uCore/internal_prog_mem/prdata
add wave -noupdate /bench/myFPGA/uCore/internal_prog_mem/pwdata
add wave -noupdate /bench/myFPGA/uCore/internal_prog_mem/pwrite
add wave -noupdate -divider bench
add wave -noupdate /bench/out_buf
add wave -noupdate /bench/sending
add wave -noupdate /bench/send_ack
add wave -noupdate /bench/host_ack
add wave -noupdate /bench/host_full
add wave -noupdate -divider Sequencer
add wave -noupdate /bench/myFPGA/uCore/uCntrl/pause
add wave -noupdate /bench/myFPGA/uCore/uCntrl/r.inst
add wave -noupdate /bench/myFPGA/uCore/prog_rdata
add wave -noupdate /bench/myFPGA/uCore/uCntrl/paddr
add wave -noupdate /bench/myFPGA/uCore/uCntrl/r.pc
add wave -noupdate -divider Datenpfad
add wave -noupdate /bench/myFPGA/uCore/uCntrl/r.tos
add wave -noupdate /bench/myFPGA/uCore/uCntrl/r.nos
add wave -noupdate /bench/myFPGA/uCore/uCntrl/ds_rdata
add wave -noupdate /bench/myFPGA/uCore/uCntrl/r.dsp
add wave -noupdate -divider {Return Stack}
add wave -noupdate /bench/myFPGA/uCore/uCntrl/r.tor
add wave -noupdate /bench/myFPGA/uCore/mem_rdata
add wave -noupdate /bench/myFPGA/uCore/uCntrl/r.rsp
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {71700 ns} 0} {{Cursor 2} {679600 ns} 0}
quietly wave cursor active 1
configure wave -namecolwidth 148
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
WaveRestoreZoom {0 ns} {325500 ns}
