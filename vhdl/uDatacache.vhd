-- ---------------------------------------------------------------------
-- @file : uDatacache.vhd
-- ---------------------------------------------------------------------
--
-- Last change: KS 15.12.2020 16:14:47
-- Project : microCore
-- Language : VHDL-2008
-- Last check in : $Rev: 619 $ $Date:: 2021-01-20 #$
-- @copyright (c): Klaus Schleisiek, All Rights Reserved.
--
-- Do not use this file except in compliance with the License. You may
-- obtain a copy of the License at http://www.microcore.org/License/.
-- Software distributed under the License is distributed on an "AS IS"
-- basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
-- See the License for the specific language governing rights and
-- limitations under the License.
--
-- @brief: Definition of the internal data memory.
--         Here fpga specific dual port memory IP can be included.
--
-- Version Author   Date       Changes
--           ks    8-Jun-2020  initial version
-- ---------------------------------------------------------------------
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE work.functions_pkg.ALL;
USE work.architecture_pkg.ALL;

ENTITY uDatacache IS PORT (
   clk          : IN  STD_LOGIC;
   enable       : IN  STD_LOGIC;
   write        : IN  STD_LOGIC;
   addr         : IN  dcache_addr;
   wdata        : IN  data_bus;
   rdata        : OUT data_bus;
   dma_enable   : IN  STD_LOGIC;
   dma_write    : IN  STD_LOGIC;
   dma_addr     : IN  dcache_addr;
   dma_wdata    : IN  data_bus;
   dma_rdata    : OUT data_bus
); END uDatacache;

ARCHITECTURE rtl OF uDatacache IS

BEGIN

make_sim_mem: IF  simulation  GENERATE

   internal_data_mem: internal_dpram
   GENERIC MAP (data_width, cache_addr_width, "rw_check", DMEM_file)
   PORT MAP (
      clk     => clk,
      ena     => enable,
      wea     => write,
      addra   => addr,
      dia     => wdata,
      doa     => rdata,
      enb     => dma_enable,
      web     => dma_write,
      addrb   => dma_addr,
      dib     => dma_wdata,
      dob     => dma_rdata
   );

END GENERATE make_sim_mem; make_syn_mem: IF  NOT simulation  GENERATE

   internal_data_mem: internal_dpram
   GENERIC MAP (data_width, cache_addr_width, "rw_check")
   PORT MAP (
      clk     => clk,
      ena     => enable,
      wea     => write,
      addra   => addr,
      dia     => wdata,
      doa     => rdata,
      enb     => dma_enable,
      web     => dma_write,
      addrb   => dma_addr,
      dib     => dma_wdata,
      dob     => dma_rdata
   );

END GENERATE make_syn_mem;

END rtl;
