-- ---------------------------------------------------------------------
-- @file : architecture_pkg.vhd
-- ---------------------------------------------------------------------
--
-- Last change: KS 24.03.2021 17:41:13
-- Last check in: $Rev: 673 $ $Date:: 2021-03-24 #$
-- @project: microCore
-- @language : VHDL-2008
-- @copyright (c): Klaus Schleisiek, All Rights Reserved.
-- @contributors :
--
-- @license: Do not use this file except in compliance with the License.
-- You may obtain a copy of the Public License at
-- https://github.com/microCore-VHDL/microCore/tree/master/documents
-- Software distributed under the License is distributed on an "AS IS"
-- basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
-- See the License for the specific language governing rights and
-- limitations under the License.
--
-- @brief: Defining all microCore parameters and records,
--         as well as the opcode fields.
--         This file is also used by the cross compiler.
--
-- Version Author   Date       Changes
--   210     ks    8-Jun-2020  initial version
--   2300    ks   12-Feb-2021  compiler switch WITH_PROG_RW eliminated.
--                             STD_LOGIC_(UN)SIGNED replaced by NUMERIC_STD
-- ---------------------------------------------------------------------
--VHDL --~  \ at this point the cross compiler activates vhdl context.
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;
USE work.functions_pkg.ALL;

PACKAGE architecture_pkg IS
--~--  \ when loaded by the microForth cross-compiler, code between "--~" up to "--~--" will be skipped.

CONSTANT version            : NATURAL := 2310; -- <major_release><functionality_added><HW_fix><SW_fix>

-- ---------------------------------------------------------------------
-- Configuration flags
-- ---------------------------------------------------------------------
-- async_reset is defined in functions_pkg.vhd, because that is the first file to load
-- CONSTANT async_reset     : BOOLEAN := false; -- true = async reset, false = synchronous reset

CONSTANT simulation         : BOOLEAN := false ; -- will e.g. increase the frequency of timers to make them observable in simulation
CONSTANT coldboot           : BOOLEAN := false ; -- cold boot on reset when true, else warmboot
CONSTANT extended           : BOOLEAN := true  ; -- false -> core instruction set, true -> extended instruction set
CONSTANT with_mult          : BOOLEAN := true  ; -- true when FPGA has hardware multiply resources
CONSTANT with_float         : BOOLEAN := true  ; -- floating point instructions?
CONSTANT with_up_download   : BOOLEAN := true  ; -- up/download via umbilical?

-- ---------------------------------------------------------------------
-- Hardware definitions
-- ---------------------------------------------------------------------
CONSTANT xtal_frequency     : NATURAL := 10000000; -- [Hz] may differ from clk_frequency
CONSTANT clk_frequency      : NATURAL := 10000000; -- [Hz] processor clock, perhaps generated by a PLL
CONSTANT cycles             : NATURAL := 1;        -- clk cycles per instruction
CONSTANT umbilical_rate     : NATURAL := 57600;
--~
-- ---------------------------------------------------------------------
-- memory initialisation during simulation
-- ---------------------------------------------------------------------

CONSTANT MEM_file           : string  := ""; -- ../software/program.mem";
CONSTANT DMEM_file          : string  := ""; -- ../software/data.mem";
--~--
-- ---------------------------------------------------------------------
-- data, cache, register, and external data memory parameters:
-- ---------------------------------------------------------------------

CONSTANT data_width         : NATURAL := 32; -- data bus width
CONSTANT exp_width          : NATURAL :=  8; -- floating point exponent width

CONSTANT data_addr_width    : NATURAL := 13; -- data memory address width, large enough for cache and external data memory
CONSTANT cache_addr_width   : NATURAL := 12; -- data cache memory address width
CONSTANT reg_addr_width     : NATURAL :=  4; -- number of address bits reserved for internal registers at the top data space
--~
CONSTANT addr_extern        : NATURAL := 2 ** cache_addr_width; -- start address of external memory
CONSTANT with_extmem        : BOOLEAN := data_addr_width /= cache_addr_width;
--~--
CONSTANT ram_data_width     : NATURAL :=  8; -- external memory word width
--~
CONSTANT chunks             : NATURAL := ceiling(data_width, ram_data_width);
CONSTANT subbits            : NATURAL := log2(chunks);
CONSTANT ram_addr_width     : NATURAL := 12 + subbits; -- external memory, virtually data_width wide
--~--
-- ---------------------------------------------------------------------
-- program memory parameters:
-- ---------------------------------------------------------------------

CONSTANT inst_width         : NATURAL :=  8; -- instruction width - this is determined by design
CONSTANT prog_addr_width    : NATURAL := 13; -- internal program memory address width
CONSTANT boot_addr_width    : NATURAL :=  6; -- size of the internal boot program memory !!! must match size of bootload.vhd !!!

CONSTANT trap_width         : NATURAL :=  3; -- each vector has room for 2**trap_width instructions

-- ---------------------------------------------------------------------
-- stacks
-- ---------------------------------------------------------------------

CONSTANT tasks_addr_width   : NATURAL :=  3; -- 2**tasks_addr_width copies of the stack areas will be provided
CONSTANT ds_addr_width      : NATURAL :=  7; -- data stack pointer width
CONSTANT rs_addr_width      : NATURAL :=  7; -- return stack pointer width
CONSTANT addr_rstack        : NATURAL := 16#C00#; -- beginning of the return stack, must be a multiple of 2**rsp_width
--~
CONSTANT addr_rstack_v      : UNSIGNED(data_width-1 DOWNTO 0) := to_unsigned(addr_rstack, data_width);
CONSTANT dsp_width          : NATURAL := ds_addr_width + tasks_addr_width;
CONSTANT rsp_width          : NATURAL := rs_addr_width + tasks_addr_width;

CONSTANT signbit            : NATURAL := data_width-1; -- signbit is used to control set/reset of bit wise writeable registers
CONSTANT octetts            : NATURAL := ceiling(data_width, 8);
--~--
CONSTANT VCC                : STD_LOGIC := '1';
CONSTANT GND                : STD_LOGIC := '0';

-- ---------------------------------------------------------------------
-- internal and memory mapped registers (addr < 0)
-- ---------------------------------------------------------------------

CONSTANT max_registers            : INTEGER :=  -1;

   CONSTANT STATUS_REG            : INTEGER :=  -1;
      CONSTANT s_c             : NATURAL :=  0;  -- carry bit
      CONSTANT s_ovfl          : NATURAL :=  1;  -- Overflow-bit of UDIVS instruction
      CONSTANT s_ie            : NATURAL :=  2;  -- Interrupt Enable bit
      CONSTANT s_iis           : NATURAL :=  3;  -- InterruptInService bit
      CONSTANT s_lit           : NATURAL :=  4;  -- LIT bit of the previous instruction
      CONSTANT s_neg           : NATURAL :=  5;  -- Sign-bit of top data element (TOS or sometimes NOS)
      CONSTANT s_zero          : NATURAL :=  6;  -- Zero-bit of top data element (TOS or sometimes NOS)
      CONSTANT s_div           : NATURAL :=  7;  -- Sign of Dividend in signed division (op_SDIVS)
      CONSTANT s_den           : NATURAL :=  8;  -- Sign of Divisor in signed division (op_SDIVS)
      CONSTANT s_unfl          : NATURAL :=  9;  -- underflow set by normalize
   CONSTANT status_width       : NATURAL := 10;

   CONSTANT DSP_REG               : INTEGER :=  -2;

   CONSTANT RSP_REG               : INTEGER :=  -3;

   CONSTANT INT_REG               : INTEGER :=  -4; -- intflags@ and ie!
      CONSTANT i_ext           : NATURAL :=  0;
   CONSTANT interrupts         : NATURAL :=  1; -- maskable interrupt sources
   CONSTANT FLAG_REG              : INTEGER :=  -5; -- flags@, pass
      CONSTANT f_dsu           : NATURAL :=  1; -- set when the dsu is connected to the umbilical (no break!)
      CONSTANT f_sema          : NATURAL :=  2; -- flag bit used as software semaphor
      CONSTANT f_bitout        : NATURAL :=  3; -- flag bit showing the state of c_bitout
   CONSTANT flag_width         : NATURAL :=  4;

   CONSTANT VERSION_REG           : INTEGER :=  -6; -- FPGA Version

   CONSTANT DEBUG_REG             : INTEGER :=  -7; -- umbilical interface

   CONSTANT TIME_REG              : INTEGER :=  -8;
      CONSTANT ticks_per_ms    : NATURAL := 4;

   CONSTANT CTRL_REG              : INTEGER :=  -9;
      CONSTANT c_bitout        : NATURAL := 0;
   CONSTANT ctrl_width         : NATURAL := 1;

CONSTANT min_registers            : INTEGER :=  -9;
--~
-- ---------------------------------------------------------------------
-- uCore subtype and record definitions
-- ---------------------------------------------------------------------

SUBTYPE byte                IS UNSIGNED ( 7 DOWNTO 0);
SUBTYPE word                IS UNSIGNED (15 DOWNTO 0);
SUBTYPE data_bus            IS UNSIGNED (data_width-1 DOWNTO 0);
SUBTYPE data_addr           IS UNSIGNED (data_addr_width-1 DOWNTO 0);
SUBTYPE dcache_addr         IS UNSIGNED (cache_addr_width-1 DOWNTO 0);
SUBTYPE exponent            IS UNSIGNED (exp_width-1 DOWNTO 0);
SUBTYPE inst_bus            IS UNSIGNED (inst_width-1 DOWNTO 0);
SUBTYPE program_addr        IS UNSIGNED (prog_addr_width-1 DOWNTO 0);
SUBTYPE boot_addr_bus       IS UNSIGNED (boot_addr_width-1 DOWNTO 0);
SUBTYPE dstacks_addr        IS UNSIGNED (dsp_width-1 DOWNTO 0);
SUBTYPE rstacks_addr        IS UNSIGNED (rsp_width-1 DOWNTO 0);
SUBTYPE int_flags           IS UNSIGNED (interrupts-1 DOWNTO 0);
SUBTYPE flag_bus            IS UNSIGNED (flag_width-1 DOWNTO 0);
SUBTYPE status_bus          IS UNSIGNED (status_width-1 DOWNTO 0);

TYPE data_sources IS ARRAY (max_registers DOWNTO min_registers) OF data_bus;

TYPE  uBus_port  IS RECORD
   reset       : STD_LOGIC;      -- synchronous, positive logic reset signal
   clk         : STD_LOGIC;      -- clock signal
   clk_en      : STD_LOGIC;      -- enable at the end of a uCore cycle
   chain       : STD_LOGIC;      -- true when executing multicycle instructions
   pause       : STD_LOGIC;      -- pause exception
   delay       : STD_LOGIC;      -- extend uCore's cycle to wait for slow peripherals
   tick        : STD_LOGIC;      -- produces a pulse every "ticks_per_ms"
   sources     : data_sources;   -- array of register outputs
-- data_io_interface
   reg_en      : STD_LOGIC;      -- enable signal for registers
   mem_en      : STD_LOGIC;      -- enable signal for dcache and return stack
   ext_en      : STD_LOGIC;      -- enable signal for external memory and IO
   write       : STD_LOGIC;      -- 1 => write, 0 => read
   addr        : data_addr;      -- address on uBus
   wdata       : data_bus;       -- data to memory
END RECORD;

TYPE  core_signals  IS RECORD
   clk_en      : STD_LOGIC;
   reg_en      : STD_LOGIC;
   mem_en      : STD_LOGIC;
   ext_en      : STD_LOGIC;      -- enable signal for external data memory
   tick        : STD_LOGIC;
   chain       : STD_LOGIC;
   status      : status_bus;
   dsp         : dstacks_addr;
   rsp         : rstacks_addr;
   int         : int_flags;
   time        : data_bus;
   debug       : data_bus;
END RECORD;

TYPE  datamem_port  IS RECORD
   enable      : STD_LOGIC;    -- internal blockRAM & external IO address space including Returnstack
   write       : STD_LOGIC;    -- 1 => write, 0 => read
   addr        : data_addr;    -- address on uBus
   wdata       : data_bus;     -- write to data memory
END RECORD;

TYPE  progmem_port  IS RECORD
   enable      : STD_LOGIC;
   write       : STD_LOGIC;    -- 1 => write, 0 => read
   read        : STD_LOGIC;    -- 1 => read program as data (2 cycle)
   addr        : program_addr;
   wdata       : inst_bus;     -- write to program memory
END RECORD;

-- ---------------------------------------------------------------------
-- umbilical debugger command bytes
-- ---------------------------------------------------------------------
--~--
CONSTANT mark_start    : byte := "00110011";
CONSTANT mark_reset    : byte := "11001100";
CONSTANT mark_debug    : byte := "10101010";
CONSTANT mark_nack     : byte := "11111111";
CONSTANT mark_ack      : byte := "00000000";
CONSTANT mark_break    : byte := "11100011";
CONSTANT mark_nbreak   : byte := "00011100";
CONSTANT mark_upload   : byte := "00001111";
CONSTANT mark_download : byte := "11110000";
--~
-- ---------------------------------------------------------------------
-- functions and components
-- ---------------------------------------------------------------------

FUNCTION uReg_read(uBus : IN uBus_port;
                   reg  : IN INTEGER
                  ) RETURN BOOLEAN;

FUNCTION uReg_write(uBus : IN uBus_port;
                    reg  : IN INTEGER
                   ) RETURN BOOLEAN;

COMPONENT semaphor PORT (
   uBus   : IN  uBus_port;
   reg    : IN  INTEGER RANGE max_registers DOWNTO min_registers; -- register read releases sema when not busy
   flag   : IN  NATURAL RANGE 0 TO flag_width-1;                  -- FLAG_REG bit that serves as semaphor
   sema   : OUT STD_LOGIC  -- next semaphor flag state
); END COMPONENT semaphor;

-- ---------------------------------------------------------------------
-- op codes in a somewhat ordered form
-- ---------------------------------------------------------------------
--
-- The opcodes' fields are defined in the instruction register as follows:
--   7   6   5   4   3   2   1   0
-- |---|---|---|---|---|---|---|---|
-- |   |   |   |   |   |   |   |   |
-- |---|---|---|---|---|---|---|---|
-- |LIT|  TYPE | STACK |    GROUP  |
-- |---|-------|-------|-----------|
--
-- ---------------------------------------------------------------------
--                    TYPE
-- ---------------------------------------------------------------------
-- Code  Name  Action
--  00   BRA   Branches, Calls and Returns
--  01   ALU   Binary and Unary Operators
--  10   MEM   Data-Memory and Register access
--  11   USR   Unused by core, free for user extensions
-- ---------------------------------------------------------------------
--                    STACK
-- ---------------------------------------------------------------------
-- Code  Name  Action
--  00   NONE  Type dependent
--  01   POP   Stack->NOS->TOS
--  10   PUSH  TOS->NOS->Stack
--  11   BOTH  Type dependent
-- ---------------------------------------------------------------------
-- Opcodes by Group
-- ---------------------------------------------------------------------
-- BRA     NONE          POP              PUSH             BOTH
--  0   00 noop       -0 drop          +0 dup           00 not
--  1   00 rot        -0 branch        +0 under         00 0=
--  2   00 -rot       -0 z-branch      +0 tuck          00 0<
--  3   00 swap       -0 nip           +0 over          00 time?
--  4                 -0 less          +0 ovfl?         00 flag?
--  5                 -0 st-set        +0 carry?        00 norm
--  6   00 PRG2NOS    -? nz-exit
--  7   00 SUM2TOS    -? tor-branch    ?0 ?dup
-- ---------------------------------------------------------------------
-- ALU     NONE          POP              PUSH             BOTH
--  0   00 mshift     -0 +             +0 2dup +        00 umult
--  1   00 mashift    -0 +c            +0 2dup +c       00 smult
--  2   00 src        -0 -             +0 2dup -        00 div
--  3   00 slc        -0 swap-         +0 2dup swap-    00 sdivs
--  4   -0 sdivl      -0 and           +0 2dup and      00 udivs
--  5   -0 udivl      -0 or            +0 2dup or       00 logs
--  6   -0 multl      -0 xor           +0 2dup xor      00 sqrts
--  7   -0 fmult      -0 +sat                           00 sqrt0
-- ---------------------------------------------------------------------
-- MEM     NONE          POP              PUSH             BOTH
--  0   00 PLUSST     -0 ST            +0 LD            00 @
--  1   00 MEM2TOR    -0 pST           +0 pLD
--  2   00 MEM2TOS
--  3   00 MEM2NOS    -0 float         +0 integ
--  4   00 LOCAL      -0 PLUSST2       +0 index         0- rdrop
--  5                                                   0- exit
--  6   -+ call                                         -- iret
--  7   -+ >r                          +0 r@            +- r>
-- ---------------------------------------------------------------------
-- USR     NONE          POP              PUSH             BOTH
--  0
--  1
--  2   0+ pause
--  3   0+ break
--  4   0+ dodoes>
--  5   0+ data!
--  6
--  7
-- ---------------------------------------------------------------------
--~--
-- BRA NONE
CONSTANT op_NOOP     : byte := "00000000";
CONSTANT op_ROT      : byte := "00000001";
CONSTANT op_NROT     : byte := "00000010";
CONSTANT op_SWAP     : byte := "00000011";


CONSTANT op_PRG2NOS  : byte := "00000110"; -- program memory load
CONSTANT op_SUM2TOS  : byte := "00000111"; -- Extended instruction set

-- BRA POP
CONSTANT op_DROP     : byte := "00001000";
CONSTANT op_BRANCH   : byte := "00001001";
CONSTANT op_QBRANCH  : byte := "00001010";
CONSTANT op_NIP      : byte := "00001011"; -- Extended instruction set
CONSTANT op_LESS     : byte := "00001100";
CONSTANT op_STSET    : byte := "00001101";
CONSTANT op_NZEXIT   : byte := "00001110"; -- Extended instruction set
CONSTANT op_NEXT     : byte := "00001111";

-- BRA PUSH
CONSTANT op_DUP      : byte := "00010000";
CONSTANT op_UNDER    : byte := "00010001"; -- Extended instruction set
CONSTANT op_TUCK     : byte := "00010010"; -- Extended instruction set
CONSTANT op_OVER     : byte := "00010011";
CONSTANT op_OVFLQ    : byte := "00010100";
CONSTANT op_CARRYQ   : byte := "00010101";

CONSTANT op_QDUP     : byte := "00010111";

-- BRA BOTH
CONSTANT op_NOT      : byte := "00011000";
CONSTANT op_ZEQU     : byte := "00011001";
CONSTANT op_ZLESS    : byte := "00011010";
CONSTANT op_TIMEQ    : byte := "00011011";
CONSTANT op_FLAGQ    : byte := "00011100"; -- Extended instruction set
CONSTANT op_NORM     : byte := "00011101"; -- WITH_FLOAT



-- ALU NONE
CONSTANT op_MSHIFT   : byte := "00100000";
CONSTANT op_MASHIFT  : byte := "00100001";
CONSTANT op_SRC      : byte := "00100010"; -- WITHOUT_MULT: shift right through carry
CONSTANT op_SLC      : byte := "00100011"; -- WITHOUT_MULT: shift left through carry
CONSTANT op_SDIVL    : byte := "00100100"; -- Extended instruction set
CONSTANT op_UDIVL    : byte := "00100101";
CONSTANT op_MULTL    : byte := "00100110";
CONSTANT op_FMULT    : byte := "00100111"; -- WITH_MULT and WITH_FLOAT

-- ALU POP
CONSTANT op_ADD      : byte := "00101000";
CONSTANT op_ADC      : byte := "00101001";
CONSTANT op_SUB      : byte := "00101010";
CONSTANT op_SSUB     : byte := "00101011";
CONSTANT op_AND      : byte := "00101100";
CONSTANT op_OR       : byte := "00101101";
CONSTANT op_XOR      : byte := "00101110";
CONSTANT op_ADDSAT   : byte := "00101111"; -- Extended instruction set

-- ALU PUSH
CONSTANT op_PADD     : byte := "00110000"; -- Extended instruction set
CONSTANT op_PADC     : byte := "00110001"; -- Extended instruction set
CONSTANT op_PSUB     : byte := "00110010"; -- Extended instruction set
CONSTANT op_PSSUB    : byte := "00110011"; -- Extended instruction set
CONSTANT op_PAND     : byte := "00110100"; -- Extended instruction set
CONSTANT op_POR      : byte := "00110101"; -- Extended instruction set
CONSTANT op_PXOR     : byte := "00110110"; -- Extended instruction set


-- ALU BOTH
CONSTANT op_UMULT    : byte := "00111000";
CONSTANT op_SMULT    : byte := "00111001"; -- WITH_MULT
CONSTANT op_DIV      : byte := "00111010";
CONSTANT op_SDIVS    : byte := "00111011"; -- Extended instruction set
CONSTANT op_UDIVS    : byte := "00111100";
CONSTANT op_LOGS     : byte := "00111101"; -- WITH_FLOAT and WITH_MULT
CONSTANT op_SQRTS    : byte := "00111110"; -- Extended instruction set
CONSTANT op_SQRT0    : byte := "00111111"; -- Extended instruction set

-- MEM NONE
CONSTANT op_PLUSST   : byte := "01000000"; -- Extended instruction set
CONSTANT op_MEM2TOR  : byte := "01000001";
CONSTANT op_MEM2TOS  : byte := "01000010"; -- Extended instruction set
CONSTANT op_MEM2NOS  : byte := "01000011";
CONSTANT op_LOCAL    : byte := "01000100";

CONSTANT op_CALL     : byte := "01000110";
CONSTANT op_RPUSH    : byte := "01000111";

-- MEM POP
CONSTANT op_STORE    : byte := "01001000";
CONSTANT op_PSTORE   : byte := "01001001"; -- program memory store

CONSTANT op_FLOAT    : byte := "01001011"; -- WITH_FLOAT
CONSTANT op_PLUSST2  : byte := "01001100"; -- Extended instruction set




-- MEM PUSH
CONSTANT op_LOAD     : byte := "01010000";
CONSTANT op_PLOAD    : byte := "01010001"; -- program memory load

CONSTANT op_INTEG    : byte := "01010011"; -- WITH_FLOAT
CONSTANT op_INDEX    : byte := "01010100"; -- Extended instruction set


CONSTANT op_RTOR     : byte := "01010111";

-- MEM BOTH
CONSTANT op_FETCH    : byte := "01011000"; -- Extended instruction set



CONSTANT op_RDROP    : byte := "01011100"; -- Extended instruction set
CONSTANT op_EXIT     : byte := "01011101";
CONSTANT op_IRET     : byte := "01011110";
CONSTANT op_RPOP     : byte := "01011111";

-- USR NONE
CONSTANT op_USR      : byte := "01100000";

CONSTANT op_PAUSE    : byte := "01100010";
CONSTANT op_BREAK    : byte := "01100011";
CONSTANT op_DOES     : byte := "01100100";
CONSTANT op_DATA     : byte := "01100101";

-- USR POP

-- USR PUSH

-- USR BOTH

--Forth

END architecture_pkg;

PACKAGE BODY architecture_pkg IS

-- ---------------------------------------------------------------------
-- uReg_read
-- returns true flag if register is addressed by reading uCore
-- ---------------------------------------------------------------------

FUNCTION uReg_read(uBus : IN uBus_port;
                   reg  : IN INTEGER
                  ) RETURN BOOLEAN IS
BEGIN
  IF  reg = signed(uBus.addr(reg_addr_width DOWNTO 0))
     AND (NOT uBus.write AND uBus.reg_en AND uBus.clk_en) = '1'
  THEN
     RETURN true;
  ELSE
     RETURN false;
  END IF;
END;

-- ---------------------------------------------------------------------
-- uReg_write
-- returns true flag if register is addressed by writing uCore
-- ---------------------------------------------------------------------

FUNCTION uReg_write (uBus : IN uBus_port;
                     reg  : IN INTEGER
                    ) RETURN BOOLEAN IS
BEGIN
  IF  reg = signed(uBus.addr(reg_addr_width DOWNTO 0))
     AND (uBus.write AND uBus.reg_en AND uBus.clk_en) = '1'
  THEN
     RETURN true;
  ELSE
     RETURN false;
  END IF;
END;

END architecture_pkg;

-- ---------------------------------------------------------------------
-- semaphor using a FLAG_REG bit
-- ---------------------------------------------------------------------

Library IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE work.architecture_pkg.ALL;
USE work.functions_pkg.async_reset;

ENTITY semaphor IS PORT (
   uBus   : IN  uBus_port;
   reg    : IN  INTEGER RANGE max_registers DOWNTO min_registers; -- register read releases sema when not busy
   flag   : IN  NATURAL RANGE 0 TO flag_width-1;                  -- FLAG_REG(flag) bit that serves as semaphor
   sema   : OUT STD_LOGIC                                         -- semaphor flag
); END semaphor;

ARCHITECTURE rtl OF semaphor IS

BEGIN

semaphor_proc : PROCESS (uBus.clk)
BEGIN
   IF  uBus.reset = '1' AND async_reset  THEN
      sema <= '0';
   ELSIF  rising_edge(uBus.clk)  THEN
      IF  uReg_write(uBus, FLAG_REG) AND (uBus.wdata(signbit) XOR uBus.wdata(flag)) = '1'  THEN
         sema <= uBus.wdata(flag);
      END IF;
      IF  uReg_write(uBus, reg)  THEN
         sema <= '1';
      ELSIF  uReg_read(uBus, reg) AND uBus.pause = '0'  THEN
         sema <= '0';
      END IF;
      IF  uBus.reset = '1' AND NOT async_reset  THEN
         sema <= '0';
      END IF;
   END IF;
END PROCESS semaphor_proc;

END rtl;

