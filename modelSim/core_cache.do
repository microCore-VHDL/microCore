onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /bench/myFPGA/reset
add wave -noupdate /bench/myFPGA/clk
add wave -noupdate /bench/myFPGA/cycle_ctr
add wave -noupdate /bench/myFPGA/clk_en
add wave -noupdate /bench/myFPGA/uBus
add wave -noupdate /bench/myFPGA/uCore/uCntrl/r
add wave -noupdate /bench/myFPGA/uCore/uCntrl/s
add wave -noupdate /bench/myFPGA/flags
add wave -noupdate /bench/myFPGA/uCore/uCntrl/pause
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
add wave -noupdate /bench/myFPGA/uCore/uCntrl/uCore_control/rstack_addr
add wave -noupdate /bench/myFPGA/uCore/uCntrl/mem_addr
add wave -noupdate -divider datamem
add wave -noupdate /bench/myFPGA/uCore/uCntrl/mem_en
add wave -noupdate /bench/myFPGA/internal_data_mem/bytes
add wave -noupdate /bench/myFPGA/uCore/uCntrl/bytes_d
add wave -noupdate -radix binary /bench/myFPGA/internal_data_mem/bytes_en
add wave -noupdate /bench/myFPGA/internal_data_mem/make_sim_mem/internal_data_mem/bytea_i
add wave -noupdate /bench/myFPGA/uBus.addr
add wave -noupdate /bench/myFPGA/internal_data_mem/make_sim_mem/internal_data_mem/addra_i
add wave -noupdate /bench/myFPGA/internal_data_mem/make_sim_mem/internal_data_mem/addra_d
add wave -noupdate /bench/myFPGA/internal_data_mem/make_sim_mem/internal_data_mem/ena
add wave -noupdate /bench/myFPGA/internal_data_mem/make_sim_mem/internal_data_mem/wea
add wave -noupdate /bench/myFPGA/uCore/uCntrl/mem_wdata
add wave -noupdate /bench/myFPGA/internal_data_mem/make_sim_mem/internal_data_mem/dia
add wave -noupdate /bench/myFPGA/uCore/uCntrl/mem_rdata
add wave -noupdate /bench/myFPGA/internal_data_mem/make_sim_mem/internal_data_mem/doa
add wave -noupdate /bench/myFPGA/internal_data_mem/make_sim_mem/internal_data_mem/ram(0)
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {1727 ns} 0} {{Cursor 2} {1790 ns} 0}
quietly wave cursor active 2
configure wave -namecolwidth 134
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
WaveRestoreZoom {1255 ns} {2831 ns}
