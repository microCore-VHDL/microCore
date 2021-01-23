-- ---------------------------------------------------------------------
-- @file : uCntrl.vhd
-- ---------------------------------------------------------------------
--
-- Last change: KS 21.01.2021 19:09:25
-- Project : microCore
-- Language : VHDL-2008
-- Last check in : $Rev: 629 $ $Date:: 2021-01-21 #$
-- @copyright (c): Klaus Schleisiek, All Rights Reserved.
--
-- Do not use this file except in compliance with the License. You may
-- obtain a copy of the License at http://www.microcore.org/License/.
-- Software distributed under the License is distributed on an "AS IS"
-- basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
-- See the License for the specific language governing rights and
-- limitations under the License.
--
-- @brief: The microCore engine and instruction decoder.
--
-- Version Author   Date       Changes
--           ks    8-Jun-2020  initial version
-- ---------------------------------------------------------------------
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_signed.ALL;
USE work.functions_pkg.ALL;
USE work.architecture_pkg.ALL;

ENTITY microcontrol IS PORT (
   uBus        : IN  uBus_port;
   deb_reset   : IN  STD_LOGIC;    -- reset issued by debugger
   deb_pause   : IN  STD_LOGIC;    -- pause issued by debugger
   deb_penable : IN  STD_LOGIC;    -- program memory ready for write by debugger
   uCtrl       : OUT core_signals;
   progmem     : OUT progmem_port;
   prog_rdata  : IN  inst_bus;     -- program memory read data
   datamem     : OUT datamem_port;
   mem_rdata   : IN  data_bus      -- data memory read data
); END microcontrol;

ARCHITECTURE rtl OF microcontrol IS

ALIAS  clk           : STD_LOGIC IS uBus.clk;
ALIAS  clk_en        : STD_LOGIC IS uBus.clk_en;
ALIAS  sources       : data_sources IS uBus.sources;
ALIAS  flags         : flag_bus     IS uBus.sources(FLAG_REG)(flag_width-1 DOWNTO 0);

SIGNAL reset         : STD_LOGIC;
SIGNAL pause         : STD_LOGIC;
SIGNAL core_en       : STD_LOGIC;
SIGNAL deb_penable_d : STD_LOGIC; -- second cycle of a program memory access

TYPE  uCore_registers  IS RECORD
   tos    : data_bus;
   nos    : data_bus;
   tor    : data_bus;
   status : status_bus;
   dsp    : dstacks_addr;
   rsp    : rstacks_addr;
   pc     : program_addr;
   inst   : inst_bus;
   chain  : STD_LOGIC;
END RECORD;

SIGNAL r             : uCore_registers;
SIGNAL r_in          : uCore_registers;
SIGNAL instruction   : inst_bus;

-- status register
TYPE  status_register  IS RECORD
   c     : STD_LOGIC; -- carry
   ovfl  : STD_LOGIC; -- integer and float overflow
   ie    : STD_LOGIC; -- interrupt enable
   iis   : STD_LOGIC; -- interrupt in service
   lit   : STD_LOGIC; -- previous instruction was lit_instruction
   neg   : STD_LOGIC; -- sign
   zero  : STD_LOGIC; -- zero
   div   : STD_LOGIC; -- sign of division
   den   : STD_LOGIC; -- sign of DividENd
   unfl  : STD_LOGIC; -- float underflow
END RECORD;

SIGNAL s            : status_register;

-- data memory bus signals
SIGNAL mem_en       : STD_LOGIC; -- select synchronous internal blockRAM
SIGNAL ext_en       : STD_LOGIC; -- select asynchronous external RAM
SIGNAL reg_en       : STD_LOGIC; -- select memory mapped register
SIGNAL mem_wr       : STD_LOGIC; -- output to memory
SIGNAL mem_addr     : data_addr; -- output to memory
SIGNAL mem_wdata    : data_bus;  -- output to memory

-- program memory
SIGNAL paddr        : program_addr; -- address for program memory
SIGNAL prog_addr    : program_addr; -- latched program memory address
SIGNAL pwrite       : STD_LOGIC;
SIGNAL pread        : STD_LOGIC;    -- read program memory as data

-- data stack
CONSTANT dsp_ini    : NATURAL := (2 ** ds_addr_width) * ((2 ** tasks_addr_width)-1);
SIGNAL ds_rdata     : data_bus;
SIGNAL ds_wdata     : data_bus;
SIGNAL ds_wr        : STD_LOGIC;
SIGNAL ds_addr      : dstacks_addr;
SIGNAL stack_addr   : dstacks_addr; -- for simulation only

-- addition
COMPONENT uAdd PORT (
   cin      : IN  STD_LOGIC;
   ladd_x   : IN  STD_LOGIC_VECTOR(data_width   DOWNTO 0);
   ladd_y   : IN  STD_LOGIC_VECTOR(data_width   DOWNTO 0);
   ladd_out : OUT STD_LOGIC_VECTOR(data_width+1 DOWNTO 0)
); END COMPONENT uAdd;

SIGNAL cin          : STD_LOGIC;
SIGNAL ladd_x       : STD_LOGIC_VECTOR(data_width   DOWNTO 0);
SIGNAL ladd_y       : STD_LOGIC_VECTOR(data_width   DOWNTO 0);
SIGNAL ladd_out     : STD_LOGIC_VECTOR(data_width+1 DOWNTO 0);
SIGNAL add_x        : data_bus;
SIGNAL add_y        : data_bus;
SIGNAL sum          : data_bus;

-- multiplication
COMPONENT uMult PORT (
   multiplicand : IN  STD_LOGIC_VECTOR(data_width DOWNTO 0);
   multiplier   : IN  STD_LOGIC_VECTOR(data_width DOWNTO 0);
   product      : OUT STD_LOGIC_VECTOR(data_width*2+1 DOWNTO 0)
); END COMPONENT uMult;

SIGNAL multiplier   : STD_LOGIC_VECTOR(data_width DOWNTO 0);
SIGNAL multiplicand : STD_LOGIC_VECTOR(data_width DOWNTO 0);
SIGNAL product      : STD_LOGIC_VECTOR(data_width*2+1 DOWNTO 0);

-- interrupts
SIGNAL ienable      : int_flags; -- enable bit register
SIGNAL pending      : int_flags; -- register of triggered interrupts
SIGNAL interrupt    : STD_LOGIC;

-- timer
CONSTANT time_cnt   : NATURAL := (clk_frequency/(1000*ticks_per_ms))-1;
SIGNAL time_ctr     : NATURAL RANGE 0 TO time_cnt; -- divides system clock
SIGNAL time         : data_bus;
SIGNAL tick         : STD_LOGIC;

BEGIN

datamem.enable    <= mem_en;
datamem.write     <= mem_wr;
datamem.addr      <= mem_addr;
datamem.wdata     <= mem_wdata;

progmem.enable    <= clk_en;
progmem.write     <= pwrite;
progmem.read      <= pread;
progmem.addr      <= paddr;
progmem.wdata     <= r.nos(inst_width-1 DOWNTO 0);

uCtrl.clk_en      <= clk_en;
uCtrl.reg_en      <= reg_en;
uCtrl.reg_addr    <= r.tos(reg_addr_width-1 DOWNTO 0);
uCtrl.ext_en      <= ext_en;
uCtrl.tick        <= tick;
uCtrl.chain       <= r.chain;
uCtrl.status      <= r.status;
uCtrl.dsp         <= r.dsp;
uCtrl.rsp         <= r.rsp;
uCtrl.int         <= pending;
uCtrl.time        <= time;
uCtrl.debug       <= (OTHERS => '0');

-- pragma translate off
s.c      <= r.status(s_c);
s.ovfl   <= r.status(s_ovfl);
s.ie     <= r.status(s_ie);
s.iis    <= r.status(s_iis);
s.lit    <= r.status(s_lit);
s.neg    <= r.status(s_neg);
s.zero   <= r.status(s_zero);
s.div    <= r.status(s_div);
s.den    <= r.status(s_den);
s.unfl   <= r.status(s_unfl);
-- pragma translate on

reset <= uBus.reset OR deb_reset;

pause <= uBus.pause OR deb_pause;

core_en <= clk_en AND NOT (deb_penable OR deb_penable_d);

-- ---------------------------------------------------------------------
-- internal registers and data stack
-- ---------------------------------------------------------------------

uReg_proc: PROCESS(clk, reset)
BEGIN

   IF  reset = '1' AND async_reset  THEN
      IF  SIMULATION  THEN
         r.tos <= to_vec(128, r.tos'length);
         r.nos <= to_vec(64, r.tos'length);
      END IF;
      r.dsp <= to_vec(dsp_ini, r.dsp'length);
      r.rsp <= (OTHERS => '1');             -- return stack grows towards lower addresses
      r.pc <= (OTHERS => '0');
      r.inst <= op_NOOP;
      r.chain <= '1';
      r.status <= (OTHERS => '0');
      deb_penable_d <= '0';
      prog_addr <= (OTHERS => '0');
      IF  simulation  THEN
         r.tor <= (OTHERS => '0');
      END IF;

   ELSIF  rising_edge(clk)  THEN

      IF  clk_en = '1'  THEN
         deb_penable_d <= deb_penable;
         prog_addr <= paddr;
         IF  pause = '1'  THEN
            r.pc <= prog_addr;          -- set back PC
            r.inst <= op_PAUSE;         -- and insert
            r.chain <= '1';             -- op_PAUSE instruction
         ELSIF  deb_penable = '1'  THEN -- while reading or writing
            r.pc <= prog_addr;          -- program memory,
            r.inst <= op_NOOP;          -- an extra NOOP cycle
            r.chain <= '1';             -- must be inserted.
         ELSE
            r <= r_in;
            IF  deb_penable_d = '1'  THEN
               r.status <= r.status;
            END IF;
         END IF;
      END IF;

      IF  reset = '1' AND NOT async_reset  THEN
         IF  SIMULATION  THEN
            r.tos <= to_vec(128, r.tos'length);
            r.nos <= to_vec(64, r.tos'length);
         END IF;
         r.dsp <= to_vec(dsp_ini, r.dsp'length);
         r.rsp <= (OTHERS => '1');          -- return stack grows towards lower addresses
         r.pc  <= (OTHERS => '0');
         r.inst <= op_NOOP;
         r.chain <= '1';
         r.status <= (OTHERS => '0');
         deb_penable_d <= '0';
         prog_addr <= (OTHERS => '0');
         IF  simulation  THEN
            r.tor <= (OTHERS => '0');
         END IF;
      END IF;

   END IF;
END PROCESS uReg_proc;

data_stack: internal_ram
GENERIC MAP (data_width, r.dsp'length, "rw_check")
PORT MAP (
   clk   => clk,
   en    => core_en,
   we    => ds_wr,
   addr  => ds_addr,
   di    => ds_wdata,
   do    => ds_rdata
);

-- pragma translate_off
stack_addr_proc : PROCESS (clk)
BEGIN
   IF  rising_edge(clk)  THEN
      IF  core_en = '1'  THEN
         stack_addr <= ds_addr; -- for simulation
      END IF;
   END IF;
END PROCESS stack_addr_proc;
-- pragma translate_on

-- ---------------------------------------------------------------------
-- interrupt management
-- ---------------------------------------------------------------------

interrupt_services: IF  interrupts /= 0  GENERATE

   interrupt_proc: PROCESS (reset, clk)
   BEGIN
      IF  reset = '1' AND async_reset  THEN
         ienable <= (OTHERS => '0');
         pending <= (OTHERS => '0');
      ELSIF  rising_edge(clk)  THEN
         pending <= flags(pending'high DOWNTO 0) AND ienable; -- synchronized twice, flags first time
         IF  core_en = '1'  THEN
            IF  uReg_write(uBus, INT_REG)  THEN
               IF  r.nos(r.nos'high) = '0'  THEN
                  ienable <= ienable OR  r.nos(ienable'high DOWNTO 0);
               ELSE
                  ienable <= ienable AND r.nos(ienable'high DOWNTO 0);
               END IF;
            END IF;
         END IF;
         IF  reset = '1' AND NOT async_reset  THEN
            ienable <= (OTHERS => '0');
            pending <= (OTHERS => '0');
         END IF;
      END IF;
   END PROCESS interrupt_proc;

   interrupt <= '1' WHEN  (r.status(s_ie) AND NOT r.status(s_iis)) = '1' AND pending /= 0  ELSE  '0';

END GENERATE interrupt_services; no_interrupt_services: IF  interrupts = 0  GENERATE

   interrupt <= '0';

END GENERATE no_interrupt_services;

-- ---------------------------------------------------------------------
-- time
-- ---------------------------------------------------------------------

timer_proc : PROCESS (clk, reset)
BEGIN
   IF  reset = '1' AND async_reset  THEN
      time <= (OTHERS => '0');
   ELSIF  rising_edge(clk)  THEN
      tick <= '0';
      IF  time_ctr = 0  THEN
         IF  simulation  THEN  time_ctr <= time_cnt/100;  ELSE  time_ctr <= time_cnt;  END IF;
         time <= time + 1;
         tick <= '1';
      ELSE
         time_ctr <= time_ctr - 1;
      END IF;
      IF  reset = '1' AND NOT async_reset  THEN
         time <= (OTHERS => '0');
      END IF;
   END IF;
END PROCESS timer_proc;

-- ---------------------------------------------------------------------
-- Adder and Multiplier
-- ---------------------------------------------------------------------

uCtrl_adder: uAdd PORT MAP (
   cin       => cin,
   ladd_x    => ladd_x,   -- data_width + 1 wide
   ladd_y    => ladd_y,   -- data_width + 1 wide
   ladd_out  => ladd_out  -- data_width + 2 wide, carry = MSBit
);

sum <= ladd_out(sum'high DOWNTO 0);

uCtrl_multiplier: uMult PORT MAP (
   multiplicand  => multiplicand, -- data_width + 1 wide
   multiplier    => multiplier,   -- data_width + 1 wide
   product       => product       -- (data_width * 2) + 1 wide
);

-- ---------------------------------------------------------------------
-- instruction decoder
-- ---------------------------------------------------------------------

instruction <= prog_rdata WHEN  r.chain = '0'  ELSE r.inst;

uCore_control: PROCESS (ALL)

   VARIABLE rsp_pop      : rstacks_addr;
   VARIABLE rsp_push     : rstacks_addr;
   VARIABLE rstack_addr  : STD_LOGIC_VECTOR(data_width-1 DOWNTO rs_addr_width);
   VARIABLE dsp_push     : dstacks_addr;
   VARIABLE dsp_pop      : dstacks_addr;
   VARIABLE nos_dspush   : dstacks_addr;
   VARIABLE tos_power2   : data_bus;
   VARIABLE temp         : data_bus;
   VARIABLE add_ovfl     : STD_LOGIC;
   VARIABLE tos_zero     : STD_LOGIC;
   VARIABLE nos_zero     : STD_LOGIC;
   VARIABLE registers    : BOOLEAN;
   VARIABLE dcache       : BOOLEAN;
   VARIABLE asyncRAM     : BOOLEAN; -- external asynchronous RAM
   VARIABLE with_extmem  : BOOLEAN;
-- floating point
   VARIABLE fexp         : exponent;
   VARIABLE mantissa     : data_bus;
   CONSTANT exp_min      : data_bus := (slice('1', data_width - exp_width + 1) & slice('0', exp_width-1));
   CONSTANT fmax_pos     : data_bus := ('0' & slice('1', data_width-1 - exp_width) & slice('1', exp_width));
   CONSTANT fmax_neg     : data_bus := ('1' & slice('0', data_width-1 - exp_width) & slice('1', exp_width));
   CONSTANT zero_pos     : data_bus := ('0' & slice('0', data_width-1));
   CONSTANT zero_neg     : data_bus := ('1' & slice('0', data_width-1));

   ALIAS lit_bit         : STD_LOGIC                    IS instruction(7);
   ALIAS i_lit           : STD_LOGIC_VECTOR(6 DOWNTO 0) IS instruction(6 DOWNTO 0);
   ALIAS i_usr           : STD_LOGIC_VECTOR(4 DOWNTO 0) IS instruction(4 DOWNTO 0);

   ALIAS add_sign        : STD_LOGIC IS ladd_out(data_width-1);
   ALIAS add_carry       : STD_LOGIC IS ladd_out(data_width  );
   ALIAS div_sign        : STD_LOGIC IS ladd_out(data_width  );
   ALIAS div_carry       : STD_LOGIC IS ladd_out(data_width+1);

   PROCEDURE set_opcode (i : IN byte) IS
   BEGIN
      r_in.inst <= i;
      r_in.chain <= '1';
      r_in.pc <= paddr;
   END set_opcode;

   PROCEDURE push_stack IS
   BEGIN
      ds_wr <= '1';
      r_in.nos <= r.tos;
      ds_wdata <= r.nos;
      ds_addr <= dsp_push;
      r_in.dsp <= dsp_push;
   END push_stack;

   PROCEDURE pop_stack IS
   BEGIN
      ds_wr <= '0';
      r_in.tos <= r.nos;
      r_in.nos <= ds_rdata;
      ds_addr <= dsp_pop;
      r_in.dsp <= dsp_pop;
   END pop_stack;

   PROCEDURE push_rstack IS
   BEGIN
      mem_wr <= '1';
      mem_wdata <= r.tor;
      mem_addr <= rstack_addr(data_addr_width-1 DOWNTO rsp_width) & rsp_push;
      r_in.rsp <= rsp_push;
      IF  addr_rstack < addr_extern  THEN
         mem_en <= '1';
      ELSE
         ext_en <= '1';
      END IF;
   END push_rstack;

   PROCEDURE pop_rstack IS
   BEGIN
      mem_addr <= rstack_addr(data_addr_width-1 DOWNTO rsp_width) & r.rsp;
      r_in.rsp <= rsp_pop;
      IF  addr_rstack < addr_extern  THEN
         mem_en <= '1';
         set_opcode(op_MEM2TOR);
      ELSE
         ext_en <= '1';
         r_in.tor <= mem_rdata;
      END IF;
   END pop_rstack;

   PROCEDURE call_trap (i : IN STD_LOGIC_VECTOR(4 DOWNTO 0)) IS
   BEGIN
      push_rstack;
      r_in.tor <= slice('0', data_width-prog_addr_width) & r.pc;
      paddr <= slice('0', (prog_addr_width - (inst_width - 3 + trap_width))) & i & slice('0', trap_width);
   END call_trap;

   PROCEDURE branch IS
   BEGIN
      IF  r.status(s_lit) = '0'  THEN
         paddr <= r.tos(paddr'high DOWNTO 0);
      ELSE
         add_x <= r.tos;
         add_y <= slice('0', data_width - prog_addr_width) & r.pc;
         paddr <= sum(paddr'high DOWNTO 0);
        END IF;
   END branch;

BEGIN

   IF  tasks_addr_width = 0  THEN
      rstack_addr := addr_rstack_v(data_width-1 DOWNTO rs_addr_width);
      dsp_push    := r.dsp + 1;
      nos_dspush  := r.nos(r.dsp'high DOWNTO 0) + 1;
      dsp_pop     := r.dsp - 1;
      rsp_pop     := r.rsp + 1;
      rsp_push    := r.rsp - 1;
   ELSE
      rstack_addr := addr_rstack_v(data_width-1 DOWNTO rsp_width) & r.rsp(rsp_width-1 DOWNTO rs_addr_width);
      dsp_push    := r.dsp(r.dsp'high DOWNTO ds_addr_width) & (r.dsp(ds_addr_width-1 DOWNTO 0) + 1);
      nos_dspush  := r.nos(r.dsp'high DOWNTO ds_addr_width) & (r.nos(ds_addr_width-1 DOWNTO 0) + 1);
      dsp_pop     := r.dsp(r.dsp'high DOWNTO ds_addr_width) & (r.dsp(ds_addr_width-1 DOWNTO 0) - 1);
      rsp_pop     := r.rsp(r.rsp'high DOWNTO rs_addr_width) & (r.rsp(rs_addr_width-1 DOWNTO 0) + 1);
      rsp_push    := r.rsp(r.rsp'high DOWNTO rs_addr_width) & (r.rsp(rs_addr_width-1 DOWNTO 0) - 1);
   END IF;

-- tos_power2 is used to barrel shift using the multiplier
   FOR  i IN 0 TO data_width-1  LOOP
      IF  r.tos(r.tos'high) = '0'  THEN     -- r.tos is positive
         IF  i = conv_INTEGER(r.tos)  THEN
            tos_power2(i) := '1';
         ELSE
            tos_power2(i) := '0';
         END IF;
      ELSE                                  -- r.tos is negative
         IF  (i - data_width) = conv_INTEGER(r.tos)  THEN
            tos_power2(i) := '1';
         ELSE
            tos_power2(i) := '0';
         END IF;
      END IF;
   END LOOP;

   tos_zero := '0';  IF  r.tos = 0  THEN  tos_zero := '1';  END IF;
   nos_zero := '0';  IF  r.nos = 0  THEN  nos_zero := '1';  END IF;

-- uCore registers
   r_in <= r;
   IF  r.status(s_lit) = '0'  THEN
      r_in.status(s_neg) <= r.tos(r.tos'high);
      r_in.status(s_zero) <= tos_zero;
   END IF;
   r_in.status(s_lit) <= '0';

-- data stack memory
   ds_wr <= '0';
   ds_addr <= r.dsp;
   ds_wdata <= r.nos;

-- arithmetic
   cin <= '0';
   add_x <= r.tos;
   add_y <= slice('0', data_width - prog_addr_width) & r.pc;
   ladd_x <= '0' & add_x;
   ladd_y <= '0' & add_y;
   add_ovfl := (add_carry XOR add_sign) AND NOT(ladd_x(r.tos'high) XOR ladd_y(r.tos'high)) AND NOT nos_zero;

   multiplicand <= '0' & r.nos;
   multiplier <= '0' & r.tos;

-- floating point
   fexp := NOT r.tos(exp_width-1) & r.tos(exp_width-2 DOWNTO 0);

-- data memory
   with_extmem := false; IF  data_addr_width > cache_addr_width                                THEN  with_extmem := true;  END IF;
   registers   := false; IF  r.tos(r.tos'high DOWNTO reg_addr_width)    = -1                   THEN    registers := true;  END IF;
   dcache      := false; IF  r.tos(r.tos'high DOWNTO cache_addr_width) = 0                     THEN       dcache := true;  END IF;
   asyncRAM    := false; IF  with_extmem AND r.tos(data_width-1 DOWNTO cache_addr_width) /= 0  THEN    asyncRAM  := true;  END IF;

   mem_en <= '0';
   reg_en <= '0';
   ext_en <= '0';
   mem_wr <= '0';
   mem_addr <= r.tos(mem_addr'high DOWNTO 0);
   mem_wdata <= r.nos;

-- program memory
   r_in.chain <= '0';
   paddr <= r.pc;
   r_in.pc <= paddr + 1;
   pwrite <= '0';
   pread <= '0';

-- ---------------------------------------------------------------------
-- interrupt
-- ---------------------------------------------------------------------

   IF  interrupt = '1' AND r.chain = '0'  THEN -- multicycle instructions are indivisible
      push_rstack;
      r_in.tor <= slice('0', data_width - prog_addr_width) & prog_addr;
      paddr <= to_vec(exp2(trap_width), prog_addr_width);
      push_stack;
      r_in.tos <= slice('0', data_width - status_width) & r.status;
      r_in.status(s_iis) <= '1';

-- ---------------------------------------------------------------------
-- literal instructions
-- ---------------------------------------------------------------------

   ELSIF  lit_bit = '1'  THEN
      r_in.status(s_lit) <= '1';
      IF  r.status(s_lit) = '0'  THEN
         push_stack;
         r_in.tos <= slice(i_lit(6), data_width - instruction'high) & i_lit;
      ELSE
         r_in.tos <= r.tos(data_width-8 DOWNTO 0) & i_lit;
      END IF;

   ELSE -- opcodes

-- ---------------------------------------------------------------------
-- single instructions
-- ---------------------------------------------------------------------

      CASE instruction IS

      WHEN op_NOOP  => NULL;

-- ---------------------------------------------------------------------
-- data stack manipulation
-- ---------------------------------------------------------------------

      WHEN op_DROP  => pop_stack;

      WHEN op_DUP   => push_stack;


      WHEN op_QDUP  => IF  r.tos /= 0  THEN -- ?DUP
                          push_stack;
                       END IF;

      WHEN op_SWAP  => r_in.nos <= r.tos;
                       r_in.tos <= r.nos;

      WHEN op_OVER  => push_stack;
                       r_in.tos <= r.nos;

      WHEN op_ROT   => r_in.tos <= ds_rdata;
                       r_in.nos <= r.tos;
                       ds_wdata <= r.nos; -- default
                       ds_wr <= '1';

      WHEN op_NROT  => r_in.tos <= r.nos;
                       r_in.nos <= ds_rdata;
                       ds_wdata <= r.tos;
                       ds_wr <= '1';

      WHEN op_NIP   => IF  extended  THEN
                          pop_stack;
                          r_in.tos <= r.tos;
                       END IF;

      WHEN op_TUCK  => IF  extended  THEN
                          push_stack;
                          r_in.nos <= r.nos;
                          ds_wdata <= r.tos;
                       END IF;

      WHEN op_UNDER => IF  extended  THEN
                          push_stack;
                          r_in.nos <= r.nos;
                       END IF;

-- ---------------------------------------------------------------------
-- return stack manipulation
-- ---------------------------------------------------------------------

      WHEN op_RPUSH => pop_stack;
                       push_rstack;
                       r_in.tor <= r.tos;

      WHEN op_RPOP  => push_stack;
                       pop_rstack;
                       r_in.tos <= r.tor;

      WHEN op_MEM2TOR => IF  addr_rstack < addr_extern  THEN       -- needed for procedure pop_rstack
                            r_in.tor <= mem_rdata;
                            r_in.status(s_lit) <= r.status(s_lit); -- I have no recollection why this is necessary - ks
                         END IF;

      WHEN op_RTOR  => push_stack;
                       r_in.tos <= r.tor;

      WHEN op_RDROP => IF  extended  THEN
                          pop_rstack;
                       END IF;

      WHEN op_SUM2TOS => IF  extended AND addr_rstack < addr_extern  THEN -- needed for op_INDEX
                            add_x <= mem_rdata;
                            add_y <= NOT r.tor;
                            cin <= '1';
                            r_in.tos <= sum;
                         END IF;

      WHEN op_INDEX => -- DO ... LOOP index computed from top two items on return stack
                       IF  extended  THEN
                          push_stack;
                          mem_addr <= rstack_addr(data_addr_width-1 DOWNTO rsp_width) & r.rsp;
                          IF  addr_rstack < addr_extern  THEN
                             mem_en <= '1';
                             set_opcode(op_SUM2TOS);
                          ELSE
                             ext_en <= '1';
                             add_x <= mem_rdata;
                             add_y <= NOT r.tor;
                             cin <= '1';
                             r_in.tos <= sum;
                          END IF;
                       END IF;

-- ---------------------------------------------------------------------
-- data memory access
-- ---------------------------------------------------------------------

      WHEN op_LOAD  => push_stack;
                       r_in.tos <= r.tos;
                       mem_addr <= r.tos(mem_addr'high DOWNTO 0);
                       IF  registers  THEN
                          reg_en <= '1';
                          r_in.nos <= sources(conv_INTEGER(r.tos(reg_addr_width DOWNTO 0)));

                          IF  r.tos(reg_addr_width-1 DOWNTO 0) = STATUS_REG  THEN
                             r_in.nos(s_lit) <= '0';
                          END IF;

                       ELSIF  asyncRAM  THEN -- external memory
                          ext_en <= '1';
                          r_in.nos <= mem_rdata;
                       ELSIF  dcache  THEN   -- internal data memory
                          mem_en <= '1';
                          set_opcode(op_MEM2NOS);
                       END IF;

      WHEN op_MEM2NOS => r_in.nos <= mem_rdata;

      WHEN op_DST2NOS => r_in.nos <= ds_rdata; -- fetch NOS from new stack location

      WHEN op_STORE => pop_stack;
                       r_in.tos <= r.tos;
                       mem_wr <= '1';
                       mem_wdata <= r.nos;
                       mem_addr <= r.tos(mem_addr'high DOWNTO 0);
                       IF  registers  THEN
                          reg_en <= '1';

                          IF  r.tos(reg_addr_width-1 DOWNTO 0) = STATUS_REG  THEN
                             r_in.status <= r.nos(r.status'high DOWNTO 0);
                          END IF;

                          IF  r.tos(reg_addr_width-1 DOWNTO 0) = DSP_REG  THEN
                             r_in.dsp <= r.nos(r.dsp'high DOWNTO 0);
                             ds_addr <= nos_dspush;
                             set_opcode(op_DST2NOS);
                          END IF;

                          IF  r.tos(reg_addr_width-1 DOWNTO 0) = RSP_REG  THEN
                             reg_en <= '0';
                             mem_wr <= '0';
                             mem_addr <= r.nos(mem_addr'high DOWNTO 0);
                             r_in.rsp <= r.nos(r.rsp'high DOWNTO 0);
                             IF  addr_rstack < addr_extern  THEN
                                mem_en <= '1';
                                set_opcode(op_MEM2TOR);
                             ELSE
                                ext_en <= '1';
                                r_in.tor <= mem_rdata;
                             END IF;
                          END IF;

                       ELSIF  asyncRAM  THEN -- external memory
                          ext_en <= '1';
                       ELSIF  dcache  THEN   -- internal data memory
                          mem_en <= '1';
                       END IF;

      WHEN op_MEM2TOS => IF  extended  THEN
                            r_in.tos <= mem_rdata;
                         END IF;

      WHEN op_FETCH => IF  extended  THEN
                          mem_addr <= r.tos(mem_addr'high DOWNTO 0);
                          IF  registers  THEN
                             reg_en <= '1';
                             r_in.tos <= sources(conv_INTEGER(r.tos(reg_addr_width DOWNTO 0)));

                             IF  r.tos(reg_addr_width-1 DOWNTO 0) = STATUS_REG  THEN
                                r_in.tos(s_lit) <= '0';
                             END IF;

                          ELSIF  asyncRAM  THEN
                             ext_en <= '1';
                             r_in.tos <= mem_rdata;
                          ELSIF  dcache  THEN   -- internal data memory
                             mem_en <= '1';
                             set_opcode(op_MEM2TOS);
                          END IF;
                       END IF;

      WHEN op_LOCAL => add_x <= r.tos - 1;
                       add_y <=    rstack_addr & r.rsp(rs_addr_width-1 DOWNTO 0);
                       r_in.tos <= rstack_addr &   sum(rs_addr_width-1 DOWNTO 0);

      WHEN op_PLUSST => -- indivisible read-modify-write +! instruction
                       IF  extended  THEN
                          mem_addr <= r.tos(mem_addr'high DOWNTO 0);
                          mem_wdata <= sum;
                          IF  registers  THEN
                             reg_en <= '1';
                             add_x <= sources(conv_INTEGER(r.tos(reg_addr_width DOWNTO 0)));
                             add_y <= r.nos;
                             cin <= '0';
                             mem_wr <= '1';
                          ELSE
                             IF  asyncRAM  THEN
                                ext_en <= '1';
                             ELSIF  dcache  THEN   -- internal data memory
                                mem_en <= '1';
                             END IF;
                             set_opcode(op_PLUSST2);
                          END IF;
                       END IF;

      WHEN op_PLUSST2 => IF  extended  THEN
                          pop_stack;
                          r_in.tos <= r.tos;
                          add_x <= mem_rdata;
                          add_y <= r.nos;
                          cin <= '0';
                          mem_addr <= r.tos(mem_addr'high DOWNTO 0);
                          mem_wdata <= sum;
                          mem_wr <= '1';
                          IF  asyncRAM  THEN
                             ext_en <= '1';
                          ELSIF  dcache  THEN   -- internal data memory
                             mem_en <= '1';
                          END IF;
                       END IF;

-- ---------------------------------------------------------------------
-- internal registers
-- ---------------------------------------------------------------------

      WHEN op_STSET  => pop_stack;
                        IF  r.tos(r.tos'high) = '1'  THEN  -- reset bits
                           r_in.status(s_c)    <= r.status(s_c)    AND r.tos(s_c);
                           r_in.status(s_ovfl) <= r.status(s_ovfl) AND r.tos(s_ovfl);
                           r_in.status(s_ie)   <= r.status(s_ie)   AND r.tos(s_ie);
                           r_in.status(s_iis)  <= r.status(s_iis)  AND r.tos(s_iis);
                        ELSE                               -- set bits
                           r_in.status(s_c)    <= r.status(s_c)    OR r.tos(s_c);
                           r_in.status(s_ovfl) <= r.status(s_ovfl) OR r.tos(s_ovfl);
                           r_in.status(s_ie)   <= r.status(s_ie)   OR r.tos(s_ie);
                           r_in.status(s_iis)  <= r.status(s_iis)  OR r.tos(s_iis);
                        END IF;

-- ---------------------------------------------------------------------
-- program memory access
-- ---------------------------------------------------------------------

      WHEN op_PRG2NOS => IF  with_prog_rw  THEN
                           r_in.nos <= slice('0', data_width - inst_width) & prog_rdata;
                        END IF;

      WHEN op_PLOAD  => IF  with_prog_rw  THEN
                           push_stack;
                           r_in.tos <= r.tos;
                           pread <= '1';
                           paddr <= r.tos(paddr'high DOWNTO 0);
                           set_opcode(op_PRG2NOS);
                           r_in.pc <= r.pc;
                        END IF;

      WHEN op_PSTORE => IF  with_prog_rw  THEN
                           pop_stack;
                           r_in.tos <= r.tos;
                           pwrite <= '1';
                           paddr <= r.tos(paddr'high DOWNTO 0);
                           set_opcode(op_NOOP);
                           r_in.pc <= r.pc;
                        END IF;

-- ---------------------------------------------------------------------
-- branches, call & exit
-- ---------------------------------------------------------------------

      WHEN op_BRANCH => pop_stack;
                        branch;

      WHEN op_QBRANCH => pop_stack;
                        IF  nos_zero = '1'  THEN
                           branch;
                        END IF;
                        set_opcode(op_DROP);

      WHEN op_NEXT   => pop_stack;
                        IF  r.tor = 0  THEN
                           pop_rstack;
                        ELSE
                           r_in.tor <= r.tor - 1;
                           branch;
                        END IF;

      WHEN op_CALL   => pop_stack;
                        push_rstack;
                        r_in.tor <= slice('0', data_width - prog_addr_width) & r.pc;
                        branch;

      WHEN op_EXIT   => pop_rstack;
                        paddr <= r.tor(paddr'high DOWNTO 0);

      WHEN op_IRET   => pop_stack;
                        pop_rstack;
                        paddr <= r.tor(paddr'high DOWNTO 0);
                        r_in.status <= r.tos(r.status'high DOWNTO 0);
                        r_in.status(s_ie) <= r.status(s_ie);  -- this way interrupts can be disabled by an interrupt
                        IF  r.tos(s_lit) = '1'  THEN
                           r_in.status(s_neg)  <= r.tos(s_neg);
                           r_in.status(s_zero) <= r.tos(s_zero);
                        END IF;

      WHEN op_PAUSE => push_rstack;
                       r_in.tor <= slice('0', data_width-prog_addr_width) & r.pc;
                       paddr <= to_vec(2 * exp2(trap_width), prog_addr_width);

      WHEN op_BREAK => call_trap(i_usr);

      WHEN op_DOES  => call_trap(i_usr);

      WHEN op_DATA  => call_trap(i_usr);

      WHEN op_NZEXIT => IF  extended  THEN
                           pop_stack;
                           IF  tos_zero = '0'  THEN
                              pop_rstack;
                              paddr <= r.tor(paddr'high DOWNTO 0);
                           END IF;
                        END IF;

-- ---------------------------------------------------------------------
-- unary arithmetic
-- ---------------------------------------------------------------------

      WHEN op_NOT   => r_in.tos <= NOT r.tos;

      WHEN op_ZEQU  => IF  tos_zero = '1'  THEN   -- Equal zero?: State of tos.
                          r_in.tos <= (OTHERS => '1');
                       ELSE
                          r_in.tos <= (OTHERS => '0');
                       END IF;

      WHEN op_ZLESS => IF  r.tos(r.tos'high) = '1'  THEN
                          r_in.tos <= (OTHERS => '1');
                       ELSE
                          r_in.tos <= (OTHERS => '0');
                       END IF;

      WHEN op_SRC   => -- shift right through carry
                       IF  NOT with_mult  THEN
                          r_in.tos <= r.status(s_c) & r.tos(r.tos'high DOWNTO 1);
                          r_in.status(s_c) <= r.tos(0);
                       END IF;

      WHEN op_SLC   => -- shift left through carry
                       IF  NOT with_mult  THEN
                          r_in.tos <= r.tos(r.tos'high-1 DOWNTO 0) & r.status(s_c);
                          r_in.status(s_c) <= r.tos(r.tos'high);
                       END IF;

      WHEN op_MSHIFT => IF  with_mult  THEN
                          multiplicand <= '0' & r.nos;
                          multiplier   <= '0' & tos_power2;
                          IF  r.tos(r.tos'high) = '0'  THEN   -- shift left
                             r_in.tos <= product(data_width*2-1 DOWNTO data_width);
                             r_in.nos <= product(data_width-1   DOWNTO          0);
                             r_in.status(s_c) <= product(data_width);
                             IF  r.tos = data_width  THEN
                                r_in.tos <= r.nos;
                                r_in.nos <= (OTHERS => '0');
                                r_in.status(s_c) <= r.nos(0);
                             END IF;
                          ELSE                                -- shift right
                             r_in.tos <= product(data_width-1   DOWNTO          0);
                             r_in.nos <= product(data_width*2-1 DOWNTO data_width);
                             r_in.status(s_c) <= product(data_width-1);
                             IF  r.tos = -data_width  THEN
                                r_in.tos <= r.nos;
                                r_in.nos <= (OTHERS => '0');
                                r_in.status(s_c) <= r.nos(r.nos'high);
                             END IF;
                          END IF;
-- without_mult: code for shift
                       ELSIF  tos_zero = '1'  THEN
                          pop_stack;
                          r_in.tos <= r.nos;
                          r_in.status(s_c) <= '0';
                       ELSE -- r.tos /= 0
                          IF  r.tos(r.tos'high) = '0'  THEN -- shift left
                             IF  r.tos = 1  THEN
                                pop_stack;
                                r_in.tos <= r.nos(r.nos'high-1 DOWNTO 0) & '0';
                                r_in.status(s_c) <= r.nos(r.nos'high);
                             ELSE
                                r_in.nos <= r.nos(r.nos'high-1 DOWNTO 0) & '0';
                                r_in.tos <= r.tos - 1;
                                paddr <= prog_addr;
                             END IF;
                          ELSE                              -- shift right
                             IF  r.tos = -1  THEN
                                pop_stack;
                                r_in.tos <= '0' & r.nos(r.nos'high DOWNTO 1);
                                r_in.status(s_c) <= r.nos(0);
                             ELSE
                                r_in.nos <= '0' & r.nos(r.nos'high DOWNTO 1);
                                r_in.tos <= r.tos + 1;
                                paddr <= prog_addr;
                             END IF;
                          END IF;
                       END IF;

      WHEN op_MASHIFT => IF  with_mult  THEN
                          multiplicand <= r.nos(r.nos'high) & r.nos;
                          multiplier   <= '0' & tos_power2;
                          IF  r.tos(r.tos'high) = '0'  THEN   -- shift left
                             r_in.tos <= product(data_width*2-1 DOWNTO data_width);
                             r_in.nos <= product(data_width-1   DOWNTO          0);
                             r_in.status(s_c) <= product(data_width);
                             IF  r.tos = data_width  THEN
                                r_in.tos <= r.nos;
                                r_in.nos <= (OTHERS => '0');
                                r_in.status(s_c) <= r.nos(0);
                             END IF;
                          ELSE                                -- shift right
                             r_in.tos <= product(data_width-1   DOWNTO          0);
                             r_in.nos <= product(data_width*2-1 DOWNTO data_width);
                             r_in.status(s_c) <= product(data_width-1);
                             IF  r.tos = -data_width  THEN
                                r_in.tos <= r.nos;
                                r_in.nos <= (OTHERS => '0');
                                r_in.status(s_c) <= r.nos(r.nos'high);
                             END IF;
                          END IF;
-- without_mult: code for ashift
                       ELSIF  tos_zero = '1'  THEN
                          pop_stack;
                          r_in.tos <= r.nos;
                          r_in.status(s_c) <= '0';
                       ELSE -- r.tos /= 0
                          IF  r.tos(r.tos'high) = '0'  THEN -- shift left
                             IF  r.tos = 1  THEN
                                pop_stack;
                                r_in.tos <= r.nos(r.nos'high-1 DOWNTO 0) & '0';
                                r_in.status(s_c) <= r.nos(r.nos'high);
                             ELSE
                                r_in.nos <= r.nos(r.nos'high-1 DOWNTO 0) & '0';
                                r_in.tos <= r.tos - 1;
                                paddr <= prog_addr;
                             END IF;
                          ELSE                              -- shift right
                             IF  r.tos = -1  THEN
                                pop_stack;
                                r_in.tos <= r.nos(r.nos'high) & r.nos(r.nos'high DOWNTO 1);
                                r_in.status(s_c) <= r.nos(0);
                             ELSE
                                r_in.nos <= r.nos(r.nos'high) & r.nos(r.nos'high DOWNTO 1);
                                r_in.tos <= r.tos + 1;
                                paddr <= prog_addr;
                             END IF;
                          END IF;
                       END IF;

-- ---------------------------------------------------------------------
-- binary arithmetic
-- ---------------------------------------------------------------------

      WHEN op_ADD  => pop_stack;
                      add_x <= r.tos;
                      add_y <= r.nos;
                      cin <= '0';
                      r_in.tos <= sum;
                      r_in.status(s_c) <= add_carry;
                      r_in.status(s_ovfl) <= add_ovfl;

      WHEN op_ADC  => pop_stack;
                      add_x <= r.tos;
                      add_y <= r.nos;
                      cin <= r.status(s_c);
                      r_in.tos <= sum;
                      r_in.status(s_c) <= add_carry;
                      r_in.status(s_ovfl) <= add_ovfl;

      WHEN op_SUB  => pop_stack;
                      add_x <= NOT r.tos;
                      add_y <= r.nos;
                      r_in.tos <= sum;
                      cin <= '1';
                      r_in.status(s_c) <= add_carry;
                      r_in.status(s_ovfl) <= add_ovfl;

      WHEN op_SSUB => pop_stack;
                      add_x <= r.tos;
                      add_y <= NOT r.nos;
                      cin <= '1';
                      r_in.tos <= sum;
                      r_in.status(s_c) <= add_carry;
                      r_in.status(s_ovfl) <= (add_carry XOR add_sign) AND NOT(ladd_x(r.tos'high) XOR ladd_y(r.tos'high)) AND NOT tos_zero;

      WHEN op_AND  => pop_stack;
                      r_in.tos <= r.tos AND r.nos;

      WHEN op_OR   => pop_stack;
                      r_in.tos <= r.tos OR  r.nos;

      WHEN op_XOR  => pop_stack;
                      r_in.tos <= r.tos XOR r.nos;

      WHEN op_ADDSAT => IF  extended  THEN
                         pop_stack;
                         add_x <= r.tos;
                         add_y <= r.nos;
                         cin <= '0';
                         r_in.status(s_c) <= add_carry;
                         r_in.status(s_ovfl) <= add_ovfl;
                         IF  add_ovfl = '1'  THEN
                            r_in.tos <= NOT sum(sum'high) & slice(sum(sum'high), data_width-1);
                         ELSE
                            r_in.tos <= sum;
                         END IF;
                      END IF;

      WHEN op_PADD => IF  extended  THEN
                         push_stack;
                         add_x <= r.tos;
                         add_y <= r.nos;
                         cin <= '0';
                         r_in.tos <= sum;
                         r_in.status(s_c) <= add_carry;
                         r_in.status(s_ovfl) <= add_ovfl;
                      END IF;

      WHEN op_PADC => IF  extended  THEN
                         push_stack;
                         add_x <= r.tos;
                         add_y <= r.nos;
                         cin <= r.status(s_c);
                         r_in.tos <= sum;
                         r_in.status(s_c) <= add_carry;
                         r_in.status(s_ovfl) <= add_ovfl;
                      END IF;

      WHEN op_PSUB => IF  extended  THEN
                         push_stack;
                         add_x <= NOT r.tos;
                         add_y <= r.nos;
                         r_in.tos <= sum;
                         cin <= '1';
                         r_in.status(s_c) <= add_carry;
                         r_in.status(s_ovfl) <= add_ovfl;
                      END IF;

      WHEN op_PSSUB=> IF  extended  THEN
                         push_stack;
                         add_x <= r.tos;
                         add_y <= NOT r.nos;
                         cin <= '1';
                         r_in.tos <= sum;
                         r_in.status(s_c) <= add_carry;
                         r_in.status(s_ovfl) <= (add_carry XOR add_sign) AND NOT(ladd_x(r.tos'high) XOR ladd_y(r.tos'high)) AND NOT tos_zero;
                      END IF;

      WHEN op_PAND => IF  extended  THEN
                         push_stack;
                         r_in.tos <= r.tos AND r.nos;
                      END IF;

      WHEN op_POR  => IF  extended  THEN
                         push_stack;
                         r_in.tos <= r.tos OR  r.nos;
                      END IF;

      WHEN op_PXOR => IF  extended  THEN
                         push_stack;
                         r_in.tos <= r.tos XOR r.nos;
                      END IF;

-- ---------------------------------------------------------------------
-- complex arithmetic
-- ---------------------------------------------------------------------

      WHEN op_UMULT => -- unsigned multiply, step instruction when mult-hardware not available
                       IF  with_mult  THEN
                          multiplicand <= '0' & r.nos;
                          multiplier   <= '0' & r.tos;
                          r_in.tos <= product(data_width*2-1 DOWNTO data_width);
                          r_in.nos <= product(data_width-1   DOWNTO          0);
                       ELSE -- multiply step instruction
                          add_x <= r.tos(r.tos'high-1 DOWNTO 0) & '0';
                          add_y <= ds_rdata;
                          IF  add_carry = '1' AND r.nos(r.nos'high) = '1'  THEN
                             r_in.nos <= (r.nos(r.nos'high-1 DOWNTO 0) & r.tos(r.tos'high)) + 1;
                          ELSE
                             r_in.nos <=  r.nos(r.nos'high-1 DOWNTO 0) & r.tos(r.tos'high);
                          END IF;
                          IF  r.nos(r.nos'high) = '0'  THEN
                             r_in.tos <= r.tos(r.tos'high-1 DOWNTO 0) & '0';
                          ELSE
                             r_in.tos <= sum;
                          END IF;
                       END IF;

      WHEN op_MULTL => -- half precision final multiply step setting overflow
                       pop_stack;
                       IF  (tos_zero = '1' AND r.nos(r.nos'high) = '0') OR (r.tos = -1 AND r.nos(r.nos'high) = '1')  THEN
                          r_in.status(s_ovfl) <= '0';
                       ELSE
                          r_in.status(s_ovfl) <= '1';
                       END IF;

      WHEN op_SMULT => -- signed multiply when mult-hardware available
                       IF  with_mult  THEN
                          multiplicand <= r.nos(r.nos'high) & r.nos;
                          multiplier <= r.tos(r.tos'high) & r.tos;
                          r_in.tos <= product(data_width*2-1 DOWNTO data_width);
                          r_in.nos <= product(data_width-1   DOWNTO          0);
                       END IF;

      WHEN op_DIV   => -- signed and unsigned division step, once for every bit
                       ladd_x <= (r.status(s_c) OR r.status(s_ovfl)) & r.tos;
                       ladd_y <= NOT('0' & ds_rdata);
                       cin <= '1';  -- sum = remainder - divisor
                       r_in.nos <= r.nos(r.nos'high-1 DOWNTO 0) & div_carry;
                       IF  div_carry = '1'  THEN
                          r_in.tos <= sum(r.tos'high-1 DOWNTO 0) & r.nos(r.nos'high);
                          r_in.status(s_c) <= sum(r.tos'high);
                       ELSE
                          r_in.tos <= r.tos(r.tos'high-1 DOWNTO 0) & r.nos(r.nos'high);
                          r_in.status(s_c) <= r.tos(r.tos'high);
                       END IF;
                       r_in.status(s_ovfl) <= NOT (div_carry XOR div_sign) OR r.status(s_ovfl);

      WHEN op_UDIVS => -- first unsigned division step
                       r_in.tos <= r.nos;    -- dividend_high -> tos
                       r_in.nos <= ds_rdata; -- dividend_low -> nos
                       ds_wdata <= r.tos;    -- divisor -> ds_rdata
                       IF  r.tos = 0  THEN
                          IF  ds_rdata = 0 AND r.nos = 0  THEN  -- special case: 0 / 0 = zero, no overflow
                             ds_wdata(0) <= '1';
                          ELSE
                             r_in.status(s_ovfl) <= '1';
                          END IF;
                       END IF;
                       ds_wr <= '1';
                       r_in.status(s_ovfl) <= '0';
                       r_in.status(s_c) <= '0';

      WHEN op_UDIVL => -- last unsigned division step
                       pop_stack;
                       ladd_x <= (r.status(s_c) OR r.status(s_ovfl)) & r.tos;
                       ladd_y <= NOT('0' & ds_rdata);
                       cin <= '1';  -- sum = remainder - divisor
                       IF  div_carry = '1'  THEN
                          r_in.nos <= sum;
                       ELSE
                          r_in.nos <= r.tos;
                       END IF;
                       r_in.tos <= r.nos(r.nos'high-1 DOWNTO 0) & div_carry;
                       r_in.status(s_ovfl) <= r.nos(r.nos'high) OR r.status(s_ovfl);

      WHEN op_SDIVS => -- first signed division step with signed divisor
                       -- ( dividend.low dividend.high divisor -- divisor dividend.low dividend.high )
                       -- dup >r   abs >r   dup 0< IF  r@ +  THEN  r> um/mod
                       -- r@ 0< IF  negate over IF  swap r@ + swap 1-  THEN THEN  rdrop
                       IF  extended  THEN
                          r_in.nos <= ds_rdata;    -- dividend_low -> NOS
                          ds_wdata <= abs(r.tos);  -- |divisor| in ds_rdata
                          ds_wr <= '1';
                          add_x <= abs(r.tos);
                          add_y <= r.nos;
                          cin <= '0';
                          IF  r.nos(r.nos'high) = '1'  THEN  -- negative dividend?
                             r_in.tos <= sum;                -- dividend_high with pre-distortion on negative
                          ELSE
                             r_in.tos <= r.nos;
                          END IF;
                          r_in.status(s_c) <= '0';
                          r_in.status(s_ovfl) <= '0';
                          r_in.status(s_div) <= r.tos(r.tos'high);
                          r_in.status(s_den) <= r.nos(r.nos'high);
                       END IF;

      WHEN op_SDIVL => -- last signed division step with signed divisor
                       -- dup >r   abs >r   dup 0< IF  r@ +  THEN  r> um/mod
                       -- r@ 0< IF  negate over IF  swap r@ + swap 1-  THEN THEN  rdrop
                       IF  extended  THEN
                          pop_stack;
                          ladd_x <= (r.status(s_c) OR r.status(s_ovfl)) & r.tos;
                          ladd_y <= NOT('0' & ds_rdata);
                          cin <= '1';  -- sum = remainder - divisor
                          IF  div_carry = '1'  THEN
                             temp := sum;   -- sum = remainder - divisor
                          ELSE
                             temp := r.tos; -- dividend_high, now remainder
                          END IF;
                          r_in.nos <= temp;
                          r_in.tos <= r.nos(r.nos'high-1 DOWNTO 0) & div_carry;
                          IF  r.status(s_div) = '1'  THEN
                             r_in.tos <= NOT (r.nos(r.nos'high-1 DOWNTO 0) & div_carry) + 1;
                             IF  temp /= 0  THEN
                                r_in.tos <= NOT (r.nos(r.nos'high-1 DOWNTO 0) & div_carry);
                                r_in.nos <= temp - ds_rdata;
                             END IF;
                          END IF;
                          -- evaluate overflow bit
                          IF    (r_in.tos(r.tos'high) = '1' AND r_in.tos(r.tos'high-1 DOWNTO 0) = 0 AND (r.status(s_div) XOR r.status(s_den)) = '0')
                             OR r.status(s_ovfl) = '1'
                             OR ((r.status(s_den) XOR r.status(s_div) XOR r_in.tos(r.tos'high)) = '1' AND r_in.tos /= 0)
                             OR r.nos(r.nos'high) = '1'
                          THEN
                             r_in.status(s_ovfl) <= '1';
                          END IF;
                       END IF;

-- : sqrt    ( u -- urem uroot )
--    0 tuck   data_width 2/
--    ?FOR  d2* d2* swap >r   swap 2* 2* 1+
--          2dup - 0< 0= IF  tuck - swap 2 +  THEN
--          u2/ swap r> swap
--    NEXT  nip swap
-- ;
-- op_SQRTS       Op: sqrts   ( complex )        don't
--             Macro: uroot   ( u -- rem root )   ?comp 0 lit, T tuck H data_width 2/ 0 DO T sqrts H LOOP T nip swap H ;
      WHEN op_SQRTS => -- square root 2bits step
                       -- root accumulated in ds_wdata
                       -- square decimated in NOS
                       -- remainder in TOS
                       IF  extended  THEN
                          add_x <= r.tos(data_width-3 DOWNTO 0) & r.nos(data_width-1 DOWNTO data_width-2);
                          add_y <= NOT (ds_rdata(data_width-3 DOWNTO 0) & "01"); -- 2s complement subtract
                          cin <= '1';
                          IF  sum(data_width-1) = '1'  THEN
                             r_in.tos <= r.tos(data_width-3 DOWNTO 0) & r.nos(data_width-1 DOWNTO data_width-2);
                             ds_wdata <= ds_rdata(data_width-2 DOWNTO 0) & '0';
                          ELSE
                             r_in.tos <= sum;
                             ds_wdata <= ds_rdata(data_width-2 DOWNTO 0) & '1';
                          END IF;
                          r_in.nos <= r.nos(data_width-3 DOWNTO 0) & "00";
                          ds_wr <= core_en;
                       END IF;

      WHEN op_SQRT0 => -- square root step for the first step of odd data_width designs
                       -- root accumulated in ds_wdata
                       -- square decimated in NOS
                       -- remainder in TOS
                       IF  extended AND to_vec(data_width, 8)(0) = '1' THEN
                          add_x <= r.tos(data_width-2 DOWNTO 0) & r.nos(data_width-1);
                          add_y <= NOT (ds_rdata(data_width-2 DOWNTO 0) & "1"); -- 2s complement subtract
                          cin <= '1';
                          IF  sum(data_width-1) = '1'  THEN
                             r_in.tos <= r.tos(data_width-2 DOWNTO 0) & r.nos(data_width-1);
                             ds_wdata <= ds_rdata(data_width-2 DOWNTO 0) & '0';
                          ELSE
                             r_in.tos <= sum;
                             ds_wdata <= ds_rdata(data_width-2 DOWNTO 0) & '1';
                          END IF;
                          r_in.nos <= r.nos(data_width-2 DOWNTO 0) & '0';
                          ds_wr <= core_en;
                       END IF;

-- ---------------------------------------------------------------------
-- flags
-- ---------------------------------------------------------------------

      WHEN op_OVFLQ  => push_stack;
                        IF  r.status(s_ovfl) = '1'  THEN
                           r_in.tos <= (OTHERS => '1');
                        ELSE
                           r_in.tos <= (OTHERS => '0');
                        END IF;

      WHEN op_CARRYQ => push_stack;
                        IF  r.status(s_c) = '1'  THEN
                           r_in.tos <= (OTHERS => '1');
                        ELSE
                           r_in.tos <= (OTHERS => '0');
                        END IF;

      WHEN op_TIMEQ => add_x <= r.tos;
                       add_y <= NOT time;
                       cin <= '0';
                       IF  add_sign = '1'  THEN
                          r_in.tos <= (OTHERS => '1');
                       ELSE
                          r_in.tos <= (OTHERS => '0');
                       END IF;

      WHEN op_LESS  => pop_stack;
                       add_x <= NOT r.tos;
                       add_y <= r.nos;
                       r_in.tos <= sum;
                       cin <= '1';
                       r_in.status(s_c) <= add_carry;
                       r_in.status(s_ovfl) <= add_ovfl;
                       IF  (add_ovfl XOR sum(sum'high)) = '1'   THEN
                          r_in.tos <= (OTHERS => '1');
                       ELSE
                          r_in.tos <= (OTHERS => '0');
                       END IF;

      WHEN op_FLAGQ  => IF  extended  THEN
                           IF  (flags AND r.tos(flags'high DOWNTO 0)) = 0  THEN
                              r_in.tos <= (OTHERS => '0');
                           ELSE
                              r_in.tos <= (OTHERS => '1');
                           END IF;
                        END IF;

-- ---------------------------------------------------------------------
-- floating point
-- ---------------------------------------------------------------------

      WHEN op_FMULT => -- fractional signed multiply with standard rounding towards even for .5
                       IF  with_float AND with_mult  THEN
                          pop_stack;
                          multiplicand <= r.nos(r.nos'high) & r.nos;
                          multiplier <= '0' & r.tos;
                          r_in.tos <= product(data_width*2-1 DOWNTO data_width);
                          IF  product(data_width-1) = '1' AND (product(data_width-2 DOWNTO 0) /= 0  OR  product(data_width) = '1')  THEN  -- round 0.5 to even
                             r_in.tos <= product(data_width*2-1 DOWNTO data_width) + 1;
                          END IF;
                       END IF;

-- : round   ( dm -- m' )
--    over 0< 0= IF  nip  EXIT THEN   \ < 0.5
--    swap 2*    IF  1+   EXIT THEN   \ > 0.5
--    dup 1 and +                     \ = 0.5, round to even
-- ;
-- : log2 ( frac -- log2[frac] )   \ Bit-wise Logarithm (K.Schleisiek/U.Lange)
--    #delta_width 0 ?DO  2*  LOOP
--    0   data_width 0
--    DO  2* >r   dup um*
--       dup 0< IF  r> 1+ >r  ELSE  d2*  THEN     \ correction of 'B(i)' and 'A(i)'
--       round   r>                               \ A(i+1):=A(i)*2^(B(i)-1)
--    LOOP  nip
-- ;
-- op_LOGS        Op: log2s   ( u  0  -- u' ld ) don't
--             Macro: ulog    ( u -- ld )        ?comp 0 lit, data_width 0 DO T log2s H LOOP T nip H ;
      WHEN op_LOGS  => -- log2 bit step
                       IF  with_float AND with_mult  THEN
                          multiplicand <= '0' & r.nos;
                          multiplier   <= '0' & r.nos;              -- nos ** 2
                          IF  product(data_width*2-1) = '0'  THEN   -- then shift nos left
                             temp := product(data_width*2-2 DOWNTO data_width-1);
                             r_in.nos <= temp;
                             IF  product(data_width-2) = '1'  THEN  -- >= 0.5?
                                IF  product(data_width-3 DOWNTO 0) /= 0 OR product(data_width-1) = '1'  THEN
                                   r_in.nos <= temp + 1;            -- round towards even
                                END IF;
                             END IF;
                          ELSE -- product(data_width*2-1) = '1'     -- don't shift nos
                             temp := product(data_width*2-1 DOWNTO data_width);
                             r_in.nos <= temp;
                             IF  product(data_width-1) = '1'  THEN  -- >= 0.5?
                                IF  product(data_width-2 DOWNTO 0) /= 0 OR product(data_width) = '1'  THEN
                                   r_in.nos <= temp + 1;            -- round towards even
                                END IF;
                             END IF;
                          END IF;
                          r_in.tos <= r.tos(data_width-2 DOWNTO 0) & product(data_width*2-1);
                       END IF;

-- : normalized?  ( m -- f )   dup #signbit and swap #signbit u2/ and 2* xor ;
-- : normalize    ( m e -- m' e' )
--    over normalized? ?EXIT
--    over 0= IF  drop   #exp_min  EXIT THEN
--    BEGIN  dup #exp_min = ?EXIT
--           1 - swap 2* swap  over normalized?
--    UNTIL
-- ;
-- op_NORM is an interruptible, multi-cycle instruction. The number of cycles depends on the mantissa argument
      WHEN op_NORM  => -- normalize a 2s-complement matissa (NOS)/exponent(TOS) number pair on the stack
                       IF  with_float  THEN
                          IF  r.nos = 0  THEN
                             r_in.tos <= slice('1', data_width - exp_width+1) & slice('0', exp_width-1); -- minimal exponent
                          ELSIF  r.nos(data_width-1) /= r.nos(data_width-2) OR r.tos = exp_min  THEN     -- already properly formatted or minimum exponent reached
                             NULL;
                          ELSE
                             r_in.tos <= r.tos - 1;
                             r_in.nos <= r.nos(r.nos'high-1 DOWNTO 0) & '0';
                             IF  r.nos(data_width-1) /= r.nos(data_width-3) OR (r.tos-1) = exp_min  THEN -- finished?
                                NULL;
                             ELSE
                                paddr <= prog_addr; -- repeat instruction
                             END IF;
                          END IF;
                       END IF;

-- : >float  ( m e -- r )   overflow off   underflow off
--    normalize   swap #man_mask and swap
--    2dup #exp_min =   swap #fzero_neg =   and >r
--    over #fzero_pos =   r> or
--    IF  drop  #exp_mask not and  EXIT THEN             \ leave floating +/-zero. For +zero irrespective of exponent
--    dup #man_mask 2/ and
--    dup 0< IF  #man_mask 2/ xor  THEN                     \ exponent over/underflow?
--    IF  0< IF  underflow on   0< IF  #fzero_neg  EXIT THEN  #fzero_pos  EXIT THEN
--        overflow on   0< IF  #fmax_neg  EXIT THEN  #fmax_pos  EXIT
--    THEN
--    dup #exp_min = IF  drop #man_mask and  EXIT THEN      \ smallest exponent => denormalized
--    #exp_mask and   #exp_sign xor   swap                  \ flip sign of exponent => bias = #exp_min
--    dup 2* [ #signbit not #exp_mask not and ] Literal and
--    swap 0< IF  #signbit or  THEN  or
-- ;
      WHEN op_FLOAT => -- convert 2s-complement mantissa(NOS)/exponent(TOS) pair to floating point number
                       IF  with_float  THEN
                          r_in.status(s_ovfl) <= '0';
                          r_in.status(s_unfl) <= '0';
                          pop_stack;
                          IF  r.nos = zero_pos OR (r.nos = zero_neg AND r.tos = exp_min)  THEN
                             r_in.tos <= r.nos(data_width-1 DOWNTO exp_width) & slice('0', exp_width);             -- leave floating +/-zero. For +zero irrespective of exponent
                          -- exponent within range?
                          ELSIF  r.tos(data_width-1 DOWNTO exp_width-1) = -1 OR r.tos(data_width-1 DOWNTO exp_width-1) = 0  THEN
                             IF  r.tos = exp_min   THEN                                                            -- minimum exponent?
                                r_in.tos <= r.nos(data_width-1 DOWNTO exp_width) & slice('0', exp_width);          -- denormalized number
                             ELSE
                                r_in.tos <= r.nos(data_width-1) & r.nos(data_width-3 DOWNTO exp_width-1) & fexp;   -- normalized number
                             END IF;
                          -- exponent out of range
                          ELSIF  r.tos(data_width-1) = '0'  THEN  -- positiv exponent?
                             r_in.status(s_ovfl) <= '1';
                             IF  r.nos(data_width-1) = '0'  THEN  -- positiv mantissa?
                                r_in.tos <= fmax_pos;
                             ELSE
                                r_in.tos <= fmax_neg;
                             END IF;
                          ELSE                                    -- negative exponent
                             r_in.status(s_unfl) <= '1';
                             IF  r.nos(data_width-1) = '0'  THEN  -- positiv mantissa?
                                r_in.tos <= zero_pos;
                             ELSE
                                r_in.tos <= zero_neg;
                             END IF;
                          END IF;
                       END IF;

-- : float>  ( r -- m e )
--    dup #exp_mask and   ?dup 0= IF  #exp_min  EXIT THEN                                \ de-normalized
--    dup #exp_sign and IF  #exp_mask 2/ and  ELSE  #exp_mask 2/ not or  THEN  swap   \ flip sign and extend
--    dup 0< IF  #exp_mask 2/ or  2/ [ #signbit #exp_sign or u2/ not ] Literal and    \ add 0.5 for better rounding
--         ELSE  #man_mask   and u2/ [ #signbit #exp_sign or u2/        ] Literal or     \ add 0.5 for better rounding
--         THEN  swap
-- ;
      WHEN op_INTEG => -- convert floating point number to 2s-complement mantissa(NOS)/exponent(TOS) pair
                       IF  with_float  THEN
                          push_stack;
                          r_in.tos <= slice(fexp(fexp'high), data_width - exp_width) & fexp;
                          IF  r.tos(exp_width-1 DOWNTO 0) = 0  THEN  -- de-normalized or zero
                             r_in.nos <= r.tos;
                          ELSIF  r.tos(data_width-1) = '0'  THEN
                             r_in.nos <= "01" & r.tos(data_width-2 DOWNTO exp_width) & '1' & slice('0', exp_width-2);  -- add 0.5 for rounding
                          ELSE
                             r_in.nos <= "10" & r.tos(data_width-2 DOWNTO exp_width) & '0' & slice('1', exp_width-2);  -- add 0.5 for rounding
                          END IF;
                       END IF;

-- ---------------------------------------------------------------------
-- user trap instructions of otherwise unused opcodes
-- ---------------------------------------------------------------------

      WHEN OTHERS   => IF  instruction(6 DOWNTO 5) = op_USR(6 DOWNTO 5)  THEN
                          call_trap(i_usr);
                       END IF;
      END CASE;

   END IF;

END PROCESS uCore_control;

END rtl;