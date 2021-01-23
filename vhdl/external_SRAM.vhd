-- ---------------------------------------------------------------------
-- @file : external_SRAM.vhd
-- ---------------------------------------------------------------------
--
-- Last change: KS 13.06.2020 19:06:43
-- Project : microCore
-- Language : VHDL-2008
-- Last check in : $Rev: 559 $ $Date:: 2020-06-13 #$
-- @copyright (c): Klaus Schleisiek, All Rights Reserved.
--
-- Do not use this file except in compliance with the License. You may
-- obtain a copy of the License at http://www.microcore.org/License/.
-- Software distributed under the License is distributed on an "AS IS"
-- basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
-- See the License for the specific language governing rights and
-- limitations under the License.
--
-- @brief: Connecting external SRAM memories to microCore. Scaled
--         by the ext_... constants in architecture_pkg.vhd
--
-- Version Author   Date       Changes
--           ks    8-Jun-2020  initial version
-- ---------------------------------------------------------------------
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_unsigned.ALL;
USE work.functions_pkg.ALL;
USE work.architecture_pkg.ALL;

ENTITY external_SRAM IS GENERIC (
   mem_addr_width : NATURAL;         -- addr width of the external SRAM
   mem_data_width : NATURAL;         -- data width of the external SRAM
   delay_cnt      : NATURAL          -- delay_cnt+1 extra clock cycles for each memory access
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
   addr        : OUT   STD_LOGIC_VECTOR(mem_addr_width-1 DOWNTO 0);
   data        : INOUT STD_LOGIC_VECTOR(mem_data_width-1 DOWNTO 0)
); END external_SRAM;

ARCHITECTURE rtl OF external_SRAM IS

ALIAS  reset           : STD_LOGIC IS uBus.reset;
ALIAS  clk             : STD_LOGIC IS uBus.clk;
ALIAS  clk_en          : STD_LOGIC IS uBus.clk_en;
ALIAS  wdata           : data_bus  IS uBus.wdata;

CONSTANT residue       : NATURAL := data_width MOD mem_data_width;
CONSTANT leader        : NATURAL := mem_data_width - residue;

SIGNAL delay_ctr       : NATURAL RANGE 0 TO max(delay_cnt, cycles-1);
SIGNAL ext_ce          : STD_LOGIC;
SIGNAL sub_addr        : STD_LOGIC_VECTOR(subbits-1 DOWNTO 0);
SIGNAL LSword          : STD_LOGIC_VECTOR((mem_data_width * (chunks-1))-1 DOWNTO 0);

BEGIN

-- ---------------------------------------------------------------------
-- mem_data_width < data_width
-- ---------------------------------------------------------------------
if_wide_data: IF  mem_data_width < data_width  GENERATE

   delay <= '1' WHEN  enable = '1' AND (ext_ce = '0' OR delay_ctr /= 0 OR sub_addr /= chunks-1)  ELSE '0';

   if_residue: IF  residue = 0  GENERATE
      ext_rdata <= data & LSword;
   END GENERATE if_residue; else_residue: IF  residue /= 0  GENERATE
      ext_rdata <= data(residue-1 DOWNTO 0) & LSword;
   END GENERATE else_residue;

   ce_n <= NOT ext_ce;
   addr <= uBus.addr(mem_addr_width - subbits-1 DOWNTO 0) & sub_addr;

   data_mux_proc: PROCESS (uBus, ext_ce, ext_memory, sub_addr)
   VARIABLE subaddr : NATURAL;
   BEGIN
      subaddr := to_NATURAL(sub_addr);
      data <= (OTHERS => 'Z');
      IF  uBus.write = '1' AND ext_ce = '1'  THEN
         IF  subaddr = chunks-1  THEN
            IF  residue = 0  THEN
               data <= wdata(wdata'high DOWNTO mem_data_width * subaddr);
            ELSE
               data <= slice('0', leader) & wdata(wdata'high DOWNTO mem_data_width * subaddr);
            END IF;
         ELSE
            data <= wdata((mem_data_width * (subaddr + 1))-1 DOWNTO mem_data_width * subaddr);
         END IF;
      END IF;
   END PROCESS data_mux_proc;

   SRAM_proc: PROCESS (clk)
   BEGIN
      IF  reset = '1' AND async_reset  THEN
         delay_ctr <= cycles - 1;
         ext_ce <= '0';
         we_n <= '1';
         oe_n <= '1';
      ELSIF  rising_edge(clk)  THEN
         IF  delay_ctr = 0  THEN
            IF  ext_ce = '0' AND enable = '1'  THEN
               delay_ctr <= delay_cnt;
               sub_addr <= (OTHERS => '0');
               ext_ce <= '1';
            END IF;
            IF  ext_ce = '1' AND sub_addr /= chunks-1  THEN
               sub_addr <= sub_addr + 1;
               delay_ctr <= delay_cnt;
               LSword <= data & LSword(LSword'high DOWNTO mem_data_width);
               we_n <= '1';
            END IF;
            IF  clk_en = '1' AND sub_addr = chunks-1 THEN
               sub_addr <= (OTHERS => '0');
               delay_ctr <= cycles - 1;
               ext_ce <= '0';
               we_n <= '1';
               oe_n <= '1';
            END IF;
         ELSIF  enable = '1'  THEN
            delay_ctr <= delay_ctr - 1;
            IF  delay_ctr = delay_cnt AND ext_ce = '1'  THEN
               IF  uBus.write = '1'  THEN
                  we_n <= '0';
               ELSE
                  oe_n <= '0';
               END IF;
            END IF;
         END IF;
         IF  reset = '1' AND NOT async_reset  THEN
            delay_ctr <= cycles - 1;
            ext_ce <= '0';
            we_n <= '1';
            oe_n <= '1';
         END IF;
      END IF;
   END PROCESS SRAM_proc;

END GENERATE if_wide_data;
-- ---------------------------------------------------------------------
-- mem_data_width >= data_width
-- ---------------------------------------------------------------------
else_wide_data: IF  mem_data_width >= data_width  GENERATE

   delay <= '1' WHEN  enable = '1' AND (ext_ce = '0' OR delay_ctr /= 0)  ELSE '0';

   ce_n <= NOT ext_ce;
   addr <= uBus.addr(mem_addr_width-1 DOWNTO 0) ;

   data_mux_proc: PROCESS (uBus, ext_ce)
   BEGIN
      data <= (OTHERS => 'Z');
      IF  uBus.write = '1' AND ext_ce = '1'  THEN
         data(data_width-1 DOWNTO 0) <= wdata;
         IF  residue /= 0  THEN
            data(mem_data_width-1 DOWNTO data_width) <= (OTHERS => '0');
         END IF;
      END IF;
   END PROCESS data_mux_proc;

   ext_rdata <= data(data_width-1 DOWNTO 0);

   SRAM_proc: PROCESS (clk)
   BEGIN
      IF  reset = '1' AND async_reset  THEN
         delay_ctr <= 0;
         ext_ce <= '0';
         we_n <= '1';
         oe_n <= '1';
      ELSIF  rising_edge(clk)  THEN
         IF  delay_ctr = 0  THEN
            IF  ext_ce = '0' AND enable = '1'  THEN
               delay_ctr <= delay_cnt;
               ext_ce <= '1';
            END IF;
            IF  ext_ce = '1'  THEN
               delay_ctr <= delay_cnt;
               we_n <= '1';
            END IF;
            IF  clk_en = '1'  THEN
               delay_ctr <= cycles - 1;
               ext_ce <= '0';
               we_n <= '1';
               oe_n <= '1';
            END IF;
         ELSIF  enable = '1'  THEN  -- AND delay_ctr /= 0
            delay_ctr <= delay_ctr - 1;
            IF  delay_ctr = delay_cnt AND ext_ce = '1'  THEN
               IF  uBus.write = '1'  THEN
                  we_n <= '0';
               ELSE
                  oe_n <= '0';
               END IF;
            END IF;
         END IF;
         IF  reset = '1' AND NOT async_reset  THEN
            delay_ctr <= 0;
            ext_ce <= '0';
            we_n <= '1';
            oe_n <= '1';
         END IF;
      END IF;
   END PROCESS SRAM_proc;

END GENERATE else_wide_data;

END rtl;
