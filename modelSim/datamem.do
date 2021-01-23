onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /bench/myFPGA/uCore/warmboot
add wave -noupdate /bench/myFPGA/reset_n
add wave -noupdate /bench/myFPGA/reset
add wave -noupdate /bench/myFPGA/clk
add wave -noupdate /bench/myFPGA/uCore/clk_en
add wave -noupdate /bench/myFPGA/uBus
add wave -noupdate /bench/myFPGA/uCore/uCntrl/r
add wave -noupdate /bench/myFPGA/uCore/uCntrl/s
add wave -noupdate -divider Sequencer
add wave -noupdate /bench/myFPGA/uCore/uCntrl/s.lit
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
add wave -noupdate -divider {Mem Interface}
add wave -noupdate /bench/myFPGA/uCore/uCntrl/reg_en
add wave -noupdate /bench/myFPGA/uCore/uCntrl/mem_en
add wave -noupdate /bench/myFPGA/uCore/uCntrl/ext_en
add wave -noupdate /bench/myFPGA/uCore/uCntrl/mem_addr
add wave -noupdate /bench/myFPGA/uCore/uCntrl/mem_rdata
add wave -noupdate /bench/myFPGA/uCore/uCntrl/mem_wdata
add wave -noupdate -divider Datamem
add wave -noupdate /bench/myFPGA/uCore/internal_data_mem/internal_data_mem/enable
add wave -noupdate /bench/myFPGA/uCore/internal_data_mem/internal_data_mem/write
add wave -noupdate /bench/myFPGA/uCore/internal_data_mem/internal_data_mem/addr
add wave -noupdate /bench/myFPGA/uCore/mem_addr
add wave -noupdate /bench/myFPGA/uCore/internal_data_mem/internal_data_mem/rdata
add wave -noupdate /bench/myFPGA/uCore/internal_data_mem/internal_data_mem/wdata
add wave -noupdate /bench/myFPGA/uCore/internal_data_mem/internal_data_mem/make_sim_mem/internal_data_mem/initialized_ram/ram(0)
add wave -noupdate /bench/myFPGA/uCore/internal_data_mem/internal_data_mem/make_sim_mem/internal_data_mem/initialized_ram/ram(1)
add wave -noupdate -divider {external Memory}
add wave -noupdate /bench/ext_ce_n
add wave -noupdate /bench/ext_we_n
add wave -noupdate /bench/ext_oe_n
add wave -noupdate /bench/ext_addr
add wave -noupdate /bench/ext_data
add wave -noupdate /bench/ext_SRAM/initialized_ram/ram(0)
add wave -noupdate /bench/ext_SRAM/initialized_ram/ram(1)
add wave -noupdate /bench/ext_SRAM/initialized_ram/ram(2)
add wave -noupdate /bench/ext_SRAM/initialized_ram/ram(3)
add wave -noupdate /bench/ext_SRAM/initialized_ram/ram(4)
add wave -noupdate /bench/ext_SRAM/initialized_ram/ram(5)
add wave -noupdate /bench/ext_SRAM/initialized_ram/ram(6)
add wave -noupdate /bench/ext_SRAM/initialized_ram/ram(7)
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {3257 ns} 0} {{Cursor 2} {10000 ns} 0}
quietly wave cursor active 1
configure wave -namecolwidth 139
configure wave -valuecolwidth 78
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
WaveRestoreZoom {0 ns} {9509 ns}
