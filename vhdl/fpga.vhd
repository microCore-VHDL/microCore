-- ---------------------------------------------------------------------
-- @file : fpga.vhd
-- ---------------------------------------------------------------------
--
-- Last change: KS 07.03.2021 22:54:43
-- Project : microCore
-- Language : VHDL-2008
-- Last check in : $Rev: 662 $ $Date:: 2021-03-10 #$
-- @copyright (c): Klaus Schleisiek, All Rights Reserved.
--
-- Do not use this file except in compliance with the License.
-- You may obtain a copy of the License at
-- https://github.com/microCore-VHDL/microCore/tree/master/documents
-- Software distributed under the License is distributed on an "AS IS"
-- basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
-- See the License for the specific language governing rights and
-- limitations under the License.
--
-- @brief: Top level microCore entity with umbilical debug interface.
--         This file should be edited for technology specific additions
--         like e.g. pad assignments and it is the source of the uBus.
--
-- Version Author   Date       Changes
--   210     ks    8-Jun-2020  initial version
--  2300     ks    8-Mar-2021  converted to NUMERIC_STD
-- ---------------------------------------------------------------------
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;
USE work.functions_pkg.ALL;
USE work.architecture_pkg.ALL;

ENTITY fpga IS PORT (
   reset_n     : IN    STD_LOGIC;
   clock       : IN    STD_LOGIC; -- external clock input
   int_n       : IN    STD_LOGIC; -- external interrupt input
   bitout      : OUT   STD_LOGIC; -- external signal for test bench synchronization
-- external SRAM
   ce_n        : OUT   STD_LOGIC;
   oe_n        : OUT   STD_LOGIC;
   we_n        : OUT   STD_LOGIC;
   addr        : OUT   UNSIGNED(mem_addr_width-1 DOWNTO 0);
   data        : INOUT UNSIGNED(ext_data_width-1 DOWNTO 0);
-- umbilical uart for debugging
   dsu_rxd     : IN    STD_LOGIC;  -- incoming asynchronous data stream
   dsu_txd     : OUT   STD_LOGIC   -- outgoing data stream
); END fpga;

ARCHITECTURE technology OF fpga IS

SIGNAL uBus       : uBus_port;
ALIAS  reset      : STD_LOGIC IS uBus.reset;
ALIAS  clk        : STD_LOGIC IS uBus.clk;
ALIAS  clk_en     : STD_LOGIC IS uBus.clk_en;

SIGNAL dsu_rxd_s  : STD_LOGIC;
SIGNAL dsu_break  : STD_LOGIC;

COMPONENT microcore PORT (
   uBus        : IN    uBus_port;
   core        : OUT   core_signals;
   ext_memory  : OUT   datamem_port;
   ext_rdata   : IN    data_bus;
   dma         : IN    datamem_port;
   dma_rdata   : OUT   data_bus;
-- umbilical uart interface
   rxd         : IN    STD_LOGIC;
   break       : OUT   STD_LOGIC;
   txd         : OUT   STD_LOGIC
); END COMPONENT microcore;

SIGNAL core         : core_signals;
SIGNAL flags        : flag_bus;
SIGNAL flags_pause  : STD_LOGIC;
SIGNAL ctrl         : UNSIGNED(ctrl_width-1 DOWNTO 0);
SIGNAL ext_memory   : datamem_port;
SIGNAL ext_rdata    : data_bus;
SIGNAL dma          : datamem_port;
SIGNAL dma_rdata    : data_bus;

COMPONENT external_SRAM GENERIC (
   mem_addr_width : NATURAL;
   ext_data_width : NATURAL;
   delay_cnt      : NATURAL    -- delay_cnt+1 extra clock cycles for each memory access
); PORT (
   uBus        : IN    uBus_port;
   enable      : IN    STD_LOGIC;    -- enable signal for specific address range
   ext_memory  : IN    datamem_port;
   ext_rdata   : OUT   data_bus;
   delay       : OUT   STD_LOGIC;
-- external SRAM
   ce_n        : OUT   STD_LOGIC;
   oe_n        : OUT   STD_LOGIC;
   we_n        : OUT   STD_LOGIC;
   addr        : OUT   UNSIGNED(mem_addr_width-1 DOWNTO 0);
   data        : INOUT UNSIGNED(ext_data_width-1 DOWNTO 0)
); END COMPONENT external_SRAM;

SIGNAL SRAM_delay   : STD_LOGIC;

BEGIN

-- ---------------------------------------------------------------------
-- input signal synchronization
-- ---------------------------------------------------------------------

synch_reset:     synchronize_n PORT MAP(clk, reset_n, reset);
synch_dsu_rxd:   synchronize   PORT MAP(clk, dsu_rxd, dsu_rxd_s);
synch_interrupt: synchronize_n PORT MAP(clk, int_n,   flags(i_ext));

-- ---------------------------------------------------------------------
-- clk generation (perhaps a PLL will be used)
-- ---------------------------------------------------------------------

clk <= clock;

-- ---------------------------------------------------------------------
-- ctrl-register (bitwise)
-- ---------------------------------------------------------------------

ctrl_proc: PROCESS (reset, clk)
BEGIN
   IF  reset = '1' AND async_reset  THEN
      ctrl <= (OTHERS => '0');
   ELSIF  rising_edge(clk)  THEN
      IF  uReg_write(uBus, CTRL_REG)  THEN
         IF  uBus.wdata(signbit) = '0'  THEN
               ctrl <= ctrl OR  uBus.wdata(ctrl'range);
         ELSE  ctrl <= ctrl AND uBus.wdata(ctrl'range);
         END IF;
      END IF;
      IF  reset = '1' AND NOT async_reset  THEN
         ctrl <= (OTHERS => '0');
      END IF;
   END IF;
END PROCESS ctrl_proc;

bitout          <= ctrl(c_bitout);
flags(f_bitout) <= ctrl(c_bitout);

uBus.sources(CTRL_REG) <= resize(ctrl, data_width);

-- ---------------------------------------------------------------------
-- software semaphor f_sema using flag register
-- ---------------------------------------------------------------------

sema_proc : PROCESS (clk, reset)
BEGIN
   IF  reset = '1' AND async_reset  THEN
      flags(f_sema) <= '0';
   ELSIF  rising_edge(clk)  THEN
      IF  uReg_write(uBus, FLAG_REG)  THEN
         IF  (uBus.wdata(signbit) XOR uBus.wdata(f_sema)) = '1'  THEN
            flags(f_sema) <= uBus.wdata(f_sema);
         END IF;
      END IF;
      IF  reset = '1' AND NOT async_reset  THEN
         flags(f_sema) <= '0';
      END IF;
   END IF;
END PROCESS sema_proc;

flags_pause <= '1' WHEN  uReg_write(uBus, FLAG_REG) AND uBus.wdata(signbit) = '0' AND
                         unsigned(uBus.wdata(flag_width-1 DOWNTO 0) AND flags) /= 0
               ELSE  '0';

-- ---------------------------------------------------------------------
-- microcore interface
-- ---------------------------------------------------------------------

dma.enable <= '0';
dma.write  <= '0';
dma.addr   <= (OTHERS => '0');
dma.wdata  <= (OTHERS => '0');

flags(f_dsu) <= NOT dsu_break; -- '1' if debug terminal present

uCore: microcore PORT MAP (
   uBus       => uBus,
   core       => core,
   ext_memory => ext_memory,
   ext_rdata  => ext_rdata,
   dma        => dma,
   dma_rdata  => dma_rdata,
-- umbilical uart interface
   rxd        => dsu_rxd_s,
   break      => dsu_break,
   txd        => dsu_txd
);

-- control signals
--ALIAS  reset        : STD_LOGIC IS uBus.reset;
--ALIAS  clk          : STD_LOGIC IS uBus.clk;
uBus.clk_en               <= core.clk_en;
uBus.chain                <= core.chain;
uBus.pause                <= flags_pause;
uBus.delay                <= SRAM_delay;
uBus.tick                 <= core.tick;
-- registers
uBus.sources(STATUS_REG)  <= resize(core.status, data_width);
uBus.sources(DSP_REG)     <= resize(core.dsp, data_width);
uBus.sources(RSP_REG)     <= addr_rstack_v(data_width-1 DOWNTO rsp_width) & core.rsp;
uBus.sources(INT_REG)     <= resize(core.int, data_width);
uBus.sources(FLAG_REG)    <= resize(flags, data_width);
uBus.sources(VERSION_REG) <= to_unsigned(version, data_width);
uBus.sources(DEBUG_REG)   <= core.debug;
uBus.sources(TIME_REG)    <= core.time;
-- data memory and return stack
uBus.reg_en               <= core.reg_en;
uBus.reg_addr             <= core.reg_addr;
uBus.enable               <= ext_memory.enable;
uBus.write                <= ext_memory.write;
uBus.addr                 <= ext_memory.addr;
uBus.wdata                <= ext_memory.wdata;
uBus.dma_rdata            <= dma_rdata;
-- ---------------------------------------------------------------------
-- external SRAM data memory
-- ---------------------------------------------------------------------

with_ext_mem: IF  data_addr_width > cache_addr_width  GENERATE

   SRAM: external_SRAM
   GENERIC MAP (mem_addr_width, ext_data_width, 2)
   PORT MAP (
      uBus        => uBus,
      enable      => ext_memory.enable,
      ext_memory  => ext_memory,
      ext_rdata   => ext_rdata,
      delay       => SRAM_delay,
   -- external SRAM
      ce_n        => ce_n,
      oe_n        => oe_n,
      we_n        => we_n,
      addr        => addr,
      data        => data
);

END GENERATE with_ext_mem; no_ext_mem: IF  data_addr_width <= cache_addr_width  GENERATE

   ext_rdata  <= (OTHERS => '0');
   SRAM_delay <= '0';

   ce_n <= '1';
   we_n <= '1';
   oe_n <= '1';
   addr <= (OTHERS => '0');
   data <= (OTHERS => 'Z');

END GENERATE no_ext_mem;

END technology;
