-- ---------------------------------------------------------------------
-- @file : functions_pkg.vhd
-- ---------------------------------------------------------------------
--
-- Last change: KS 08.03.2021 11:10:37
-- Project : microCore
-- Language : VHDL-2008
-- Last check in : $Rev: 657 $ $Date:: 2021-03-08 #$
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
-- @brief: Functions and components which are used throughout microCore.
--
-- Version Author   Date       Changes
--   210     ks    8-Jun-2020  initial version
--  2300     ks    8-Mar-2021  Converted to NUMERIC_STD
-- ---------------------------------------------------------------------
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;
USE STD.TEXTIO.ALL;

PACKAGE functions_pkg IS

CONSTANT async_reset : BOOLEAN := false; -- true = async reset, false = synchronous reset

FUNCTION  resize(slv : IN STD_LOGIC_VECTOR;
                 s   : IN INTEGER        ) RETURN STD_LOGIC_VECTOR;

FUNCTION   slice(v : IN STD_LOGIC;
                 s : IN INTEGER          ) RETURN UNSIGNED;

FUNCTION     max(v : IN INTEGER;
                 w : IN INTEGER          ) RETURN INTEGER;

FUNCTION    log2(v : IN INTEGER          ) RETURN INTEGER;

FUNCTION    exp2(v : IN INTEGER          ) RETURN INTEGER;

FUNCTION ceiling(v : IN INTEGER;
                 w : IN INTEGER          ) RETURN INTEGER;

FUNCTION next_quad(width : IN NATURAL    ) RETURN NATURAL;

COMPONENT synchronize
PORT (clk      : IN    STD_LOGIC;
      async    : IN    STD_LOGIC;
      sync     : OUT   STD_LOGIC);
END COMPONENT;

COMPONENT synchronize_n
PORT (clk      : IN    STD_LOGIC;
      async_n  : IN    STD_LOGIC;
      sync     : OUT   STD_LOGIC);
END COMPONENT;

COMPONENT debounce
GENERIC (width : INTEGER := 5);
PORT (reset    : IN    STD_LOGIC;
      clk      : IN    STD_LOGIC;
      clk_en   : IN    STD_LOGIC;
      async    : IN    STD_LOGIC;
      sync     : OUT   STD_LOGIC);
END COMPONENT;

COMPONENT debounce_n
GENERIC (width : INTEGER := 5);
PORT (reset    : IN    STD_LOGIC;
      clk      : IN    STD_LOGIC;
      clk_en   : IN    STD_LOGIC;
      async    : IN    STD_LOGIC;
      sync     : OUT   STD_LOGIC);
END COMPONENT;

COMPONENT edge
PORT (reset   : IN  STD_LOGIC;
      clk     : IN  STD_LOGIC;
      i       : IN  STD_LOGIC;
      o       : OUT STD_LOGIC);
END COMPONENT;

COMPONENT edge_n
PORT (reset   : IN  STD_LOGIC;
      clk     : IN  STD_LOGIC;
      i       : IN  STD_LOGIC;
      o       : OUT STD_LOGIC);
END COMPONENT;

COMPONENT asynch_dpram
GENERIC (data_width  : INTEGER;
         addr_width  : INTEGER);
PORT (clk   : IN  STD_LOGIC;
      we    : IN  STD_LOGIC;
      waddr : IN  UNSIGNED(addr_width-1 DOWNTO 0);
      di    : IN  UNSIGNED(data_width-1 DOWNTO 0);
      raddr : IN  UNSIGNED(addr_width-1 DOWNTO 0);
      do    : OUT UNSIGNED(data_width-1 DOWNTO 0));
END COMPONENT;

COMPONENT asynch_ram
GENERIC (data_width : INTEGER;
         addr_width : INTEGER);
PORT (clk   : IN    STD_LOGIC;
      we    : IN    STD_LOGIC;
      addr  : IN    UNSIGNED;
      di    : IN    UNSIGNED;
      do    : OUT   UNSIGNED);
END COMPONENT;

COMPONENT internal_ram GENERIC (
   data_width : INTEGER;
   addr_width : INTEGER;
   ramstyle   : STRING := "block_ram";
   init_file  : STRING := "none"
); PORT (
   clk   : IN    STD_LOGIC;
   en    : IN    STD_LOGIC;
   we    : IN    STD_LOGIC;
   addr  : IN    UNSIGNED;
   di    : IN    UNSIGNED;
   do    : OUT   UNSIGNED
); END COMPONENT;

COMPONENT internal_dpram GENERIC (
   data_width : INTEGER;
   addr_width : INTEGER;
   ramstyle   : STRING := "block_ram";
   init_file  : STRING := "none"
); PORT (
   clk   : IN    STD_LOGIC;
   ena   : IN    STD_LOGIC;
   wea   : IN    STD_LOGIC;
   addra : IN    UNSIGNED;
   dia   : IN    UNSIGNED;
   doa   : OUT   UNSIGNED;
   enb   : IN    STD_LOGIC;
   web   : IN    STD_LOGIC;
   addrb : IN    UNSIGNED;
   dib   : IN    UNSIGNED;
   dob   : OUT   UNSIGNED
); END COMPONENT;

COMPONENT external_ram GENERIC (
   data_width : INTEGER;
   addr_width : INTEGER;
   init_file  : STRING := "none"
); PORT (
   ce_n   : IN    STD_LOGIC;
   oe_n   : IN    STD_LOGIC;
   we_n   : IN    STD_LOGIC;
   addr   : IN    UNSIGNED;
   data   : INOUT UNSIGNED
); END COMPONENT;

COMPONENT monoflop GENERIC (
   ptime : integer         -- Pulsdauer in clk Perioden
); PORT (
   clk    : IN STD_LOGIC;  -- Input Clock
   reset  : IN STD_LOGIC;  -- Async. Reset
   inrt   : IN STD_LOGIC;  -- retriggerbarer Input
   pout   : OUT STD_LOGIC  -- Output Pulse
); END COMPONENT;

COMPONENT fifo GENERIC (
   width    : NATURAL;
   depth    : NATURAL;
   ramstyle : STRING := "registers"
); PORT (
   reset    : IN  STD_LOGIC;
   clk      : IN  STD_LOGIC;
   push     : IN  STD_LOGIC;
   pop      : IN  STD_LOGIC;
   empty    : OUT STD_LOGIC;
   full     : OUT STD_LOGIC;
   din      : IN  UNSIGNED(width-1 DOWNTO 0);
   dout     : OUT UNSIGNED(width-1 DOWNTO 0)
); END COMPONENT;

END functions_pkg;

PACKAGE BODY functions_pkg IS

   -- ------------------------------------------------------------------
   -- resize
   -- resize STD_LOGIC_VECTOR to STD_LOGIC_VECTOR, s bits long
   -- ------------------------------------------------------------------

   FUNCTION  resize(slv : IN STD_LOGIC_VECTOR;
                    s   : IN INTEGER        ) RETURN STD_LOGIC_VECTOR IS
      VARIABLE temp   : UNSIGNED(slv'range);
      VARIABLE result : UNSIGNED(s-1 DOWNTO 0);
   BEGIN
      temp := unsigned(slv);
      result := resize(temp, s);
      RETURN std_logic_vector(result);
   END;

   -- ------------------------------------------------------------------
   -- slice
   -- Return (others => v) within range s-1 to 0
   -- ------------------------------------------------------------------

   FUNCTION slice(v : IN STD_LOGIC;
                  s : IN INTEGER
                 )   RETURN UNSIGNED IS
      VARIABLE temp : UNSIGNED (s-1 DOWNTO 0);
   BEGIN
       temp := (OTHERS => v);
      RETURN temp;
   END;

   -- ------------------------------------------------------------------
   -- max
   -- Return maximum of two INTEGERs w and v
   -- ------------------------------------------------------------------

   FUNCTION max(v  :  IN INTEGER;
                w  :  IN INTEGER
               ) RETURN INTEGER IS
   BEGIN
       IF (v > w) THEN  RETURN v;  ELSE  RETURN w;  END IF;
   END;

   -- ------------------------------------------------------------------
   -- logarithm dualis
   -- ------------------------------------------------------------------

   FUNCTION log2(v    :  IN INTEGER) RETURN INTEGER IS
       VARIABLE temp  : INTEGER;
   BEGIN
       temp := 0;
       WHILE  2 ** temp < v  LOOP
           temp := temp + 1;
       END LOOP;
       RETURN temp;
   END;

   -- ------------------------------------------------------------------
   -- exponentiation dualis
   -- ------------------------------------------------------------------

   FUNCTION exp2(v    :  IN INTEGER) RETURN INTEGER IS
       VARIABLE temp  : INTEGER;
   BEGIN
       temp := 1;
       IF  v /= 0  THEN
          FOR i IN v DOWNTO 1  LOOP
             temp := temp*2;
          END LOOP;
       END IF;
       RETURN temp;
   END;

   -- ------------------------------------------------------------------
   -- ceiling of two number
   -- ------------------------------------------------------------------

   FUNCTION ceiling(v : IN INTEGER;
                    w : IN INTEGER
                   ) RETURN INTEGER IS
       VARIABLE temp  : INTEGER;
   BEGIN
       temp := v/w;
       IF  temp * w < v  THEN
          temp := temp + 1;
       END IF;
       RETURN temp;
   END;

   -- ------------------------------------------------------------------
   -- next length, which is a multiple of 4 (quad)
   -- ------------------------------------------------------------------

   FUNCTION next_quad(width : IN NATURAL) RETURN NATURAL IS
      VARIABLE len : INTEGER := 0;
   BEGIN
      len := 0;
      LOOP
         len := len + 4;
         IF  len >= width  THEN  EXIT; END IF;
      END LOOP;
      RETURN len;
   END;

END functions_pkg;

-- ---------------------------------------------------------------------
-- synchronising asynchronous input signals with 2 FFs
-- ---------------------------------------------------------------------

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

ENTITY synchronize IS
PORT (clk      : IN    STD_LOGIC;
      async    : IN    STD_LOGIC;
      sync     : OUT   STD_LOGIC);
END synchronize;

ARCHITECTURE rtl OF synchronize IS

SIGNAL temp    : UNSIGNED(1 DOWNTO 0) := "01";

BEGIN

sync <= temp(1);

synch: PROCESS(clk)
BEGIN
   IF  rising_edge(clk)   THEN
      temp <= temp(0) & async;
   END IF;
END PROCESS synch;

END rtl;

-- ---------------------------------------------------------------------
-- synchronising inverted asynchronous input signals with 2 FFs
-- ---------------------------------------------------------------------

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

ENTITY synchronize_n IS
PORT (clk      : IN    STD_LOGIC;
      async_n  : IN    STD_LOGIC;
      sync     : OUT   STD_LOGIC);
END synchronize_n;

ARCHITECTURE rtl OF synchronize_n IS

SIGNAL temp    : UNSIGNED(1 DOWNTO 0) := "10";

BEGIN

sync <= temp(1);

synch: PROCESS(clk)
BEGIN
   IF  rising_edge(clk)   THEN
      temp <= NOT temp(0) & async_n;
   END IF;
END PROCESS synch;

END rtl;

-- ---------------------------------------------------------------------
-- debouncing asynchronous input signals
-- ---------------------------------------------------------------------

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;
USE work.functions_pkg.async_reset;

ENTITY debounce IS
GENERIC (width : INTEGER);
PORT (reset    : IN    STD_LOGIC;
      clk      : IN    STD_LOGIC;
      clk_en   : IN    STD_LOGIC;
      async    : IN    STD_LOGIC;
      sync     : OUT   STD_LOGIC);
END debounce;

ARCHITECTURE rtl OF debounce IS
SIGNAL temp        : UNSIGNED(width-2 DOWNTO 0);
CONSTANT allones   : UNSIGNED(width-1 DOWNTO 0) := (OTHERS => '1');
CONSTANT allzeroes : UNSIGNED(width-1 DOWNTO 0) := (OTHERS => '0');
BEGIN

noglitch: PROCESS(clk, reset)
BEGIN
   IF  reset = '1' AND async_reset  THEN
      sync <= '0';
      temp <= (OTHERS => '0');
   ELSIF  rising_edge(clk)  THEN
      IF  clk_en = '1'  THEN
         temp <= temp(temp'high-1 DOWNTO 0) & async;
         IF  (temp&async) = allones  THEN
            sync <= '1';
         END IF;
         IF  (temp&async) = allzeroes  THEN
            sync <= '0';
         END IF;
      END IF;
      IF  reset = '1' AND NOT async_reset  THEN
         sync <= '0';
         temp <= (OTHERS => '0');
      END IF;
   END IF;
END PROCESS noglitch;

END rtl;

-- ---------------------------------------------------------------------
-- debouncing inverted asynchronous input signals
-- ---------------------------------------------------------------------

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;
USE work.functions_pkg.async_reset;

ENTITY debounce_n IS
GENERIC (width : INTEGER);
PORT (reset    : IN    STD_LOGIC;
      clk      : IN    STD_LOGIC;
      clk_en   : IN    STD_LOGIC;
      async    : IN    STD_LOGIC;
      sync     : OUT   STD_LOGIC);
END debounce_n;

ARCHITECTURE rtl OF debounce_n IS
SIGNAL temp        : UNSIGNED(width-2 DOWNTO 0);
CONSTANT allones   : UNSIGNED(width-1 DOWNTO 0) := (OTHERS => '1');
CONSTANT allzeroes : UNSIGNED(width-1 DOWNTO 0) := (OTHERS => '0');
BEGIN

noglitch_n: PROCESS(clk, reset)
BEGIN
   IF  reset = '1' AND async_reset  THEN
      sync <= '0';
      temp <= (OTHERS => '0');
   ELSIF  rising_edge(clk)  THEN
      IF  clk_en = '1'  THEN
         temp <= temp(temp'high-1 DOWNTO 0) & async;
         IF  (temp&async) = allones  THEN
            sync <= '0';
         END IF;
         IF  (temp&async) = allzeroes  THEN
            sync <= '1';
         END IF;
      END IF;
      IF  reset = '1' AND NOT async_reset  THEN
         sync <= '0';
         temp <= (OTHERS => '0');
      END IF;
   END IF;
END PROCESS noglitch_n;

END rtl;

-- ---------------------------------------------------------------------
-- metastable safe positive edge detector
-- ---------------------------------------------------------------------

Library IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;
USE work.functions_pkg.async_reset;

ENTITY edge IS
     PORT (reset   : IN  STD_LOGIC;
           clk     : IN  STD_LOGIC;
           i       : IN  STD_LOGIC;
           o       : OUT STD_LOGIC);
END edge;

ARCHITECTURE rtl OF edge IS

SIGNAL temp   : STD_LOGIC;
SIGNAL output : STD_LOGIC;

BEGIN

o <= output;

risingedge: PROCESS(reset, clk)
BEGIN
   IF  reset = '1' AND async_reset  THEN
      temp   <= '0';          -- temp
      output <= '0';          -- out
   ELSIF  rising_edge(clk)   THEN
      temp   <= i AND     (temp OR output);
      output <= i AND NOT (temp OR output);
      IF  reset = '1' AND NOT async_reset  THEN
         temp   <= '0';       -- temp
         output <= '0';       -- out
      END IF;
   END IF;
END PROCESS;

END rtl;

-- ---------------------------------------------------------------------
-- metastable safe negative edge detector
-- ---------------------------------------------------------------------

Library IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;
USE work.functions_pkg.async_reset;

ENTITY edge_n IS
     PORT (reset   : IN  STD_LOGIC;
           clk     : IN  STD_LOGIC;
           i       : IN  STD_LOGIC;
           o       : OUT STD_LOGIC);
END edge_n;

ARCHITECTURE rtl OF edge_n IS

SIGNAL temp   : STD_LOGIC;
SIGNAL output : STD_LOGIC;

BEGIN

o   <= output;

fallingedge: PROCESS(reset, clk)
BEGIN
   IF  reset = '1' AND async_reset  THEN
      temp   <= '1';          -- temp
      output <= '0';          -- out
   ELSIF  rising_edge(clk)   THEN
      temp   <= NOT i AND     (temp OR output);
      output <= NOT i AND NOT (temp OR output);
      IF  reset = '1' AND NOT async_reset  THEN
         temp   <= '1';       -- temp
         output <= '0';       -- out
      END IF;
   END IF;
END PROCESS;

END rtl;

-- ---------------------------------------------------------------------
-- internal dual-port asynchronous RAM made of LUTs
-- ---------------------------------------------------------------------

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;
USE work.functions_pkg.ALL;

ENTITY asynch_dpram IS
GENERIC (data_width  : INTEGER;
         addr_width  : INTEGER);
PORT (clk   : IN  STD_LOGIC;
      we    : IN  STD_LOGIC;
      waddr : IN  UNSIGNED(addr_width-1 DOWNTO 0);
      di    : IN  UNSIGNED(data_width-1 DOWNTO 0);
      raddr : IN  UNSIGNED(addr_width-1 DOWNTO 0);
      do    : OUT UNSIGNED(data_width-1 DOWNTO 0));
END asynch_dpram;

ARCHITECTURE inference_model OF asynch_dpram IS

TYPE ram_type IS ARRAY (2**addr_width-1 DOWNTO 0) OF UNSIGNED(data_width-1 DOWNTO 0);

SIGNAL ram : ram_type;

BEGIN

asyn_dpram: PROCESS(clk)
BEGIN
  IF  rising_edge(clk)   THEN
     IF  we = '1'  THEN
        ram(to_integer(waddr)) <= di ;
     END IF;
  END IF;
END PROCESS asyn_dpram;

do <= ram(to_integer(raddr));

END inference_model;

-- ---------------------------------------------------------------------
-- internal asynchronous RAM made of LUTs
-- ---------------------------------------------------------------------

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;
USE work.functions_pkg.ALL;

ENTITY asynch_ram IS
GENERIC (data_width  : INTEGER;
         addr_width  : INTEGER);
PORT (clk   : IN  STD_LOGIC;
      we    : IN  STD_LOGIC;
      addr  : IN  UNSIGNED(addr_width-1 DOWNTO 0);
      di    : IN  UNSIGNED(data_width-1 DOWNTO 0);
      do    : OUT UNSIGNED(data_width-1 DOWNTO 0));
END asynch_ram;

ARCHITECTURE inference_model OF asynch_ram IS

TYPE ram_type IS ARRAY (2**addr_width-1 DOWNTO 0) OF UNSIGNED(data_width-1 DOWNTO 0);

SIGNAL ram : ram_type;

BEGIN

asyn_ram: PROCESS(clk)
BEGIN
  IF  rising_edge(clk)   THEN
     IF  we = '1'  THEN
        ram(to_integer(addr)) <= di ;
     END IF;
  END IF;
END PROCESS asyn_ram;

do <= ram(to_integer(addr));

END inference_model;

-- ---------------------------------------------------------------------
-- internal_blockRAM:
-- Address latched prior to access, input data stored on rising clock edge.
-- During simulation initialized by an ascii-file of the following format:
-- addr   0        1        2        3
-- 0000 : 95930800 00000000 94F4471F 07000000
-- ---------------------------------------------------------------------

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;
USE STD.TEXTIO.ALL;
USE work.functions_pkg.ALL;

ENTITY internal_ram IS GENERIC (
   data_width : INTEGER;
   addr_width : INTEGER;
   ramstyle   : STRING;
   init_file  : STRING
); PORT (
   clk   : IN    STD_LOGIC;
   en    : IN    STD_LOGIC;
   we    : IN    STD_LOGIC;
   addr  : IN    UNSIGNED(addr_width-1 DOWNTO 0);
   di    : IN    UNSIGNED(data_width-1 DOWNTO 0);
   do    : OUT   UNSIGNED(data_width-1 DOWNTO 0)
); END internal_ram;

ARCHITECTURE inference_model OF internal_ram IS

ATTRIBUTE syn_ramstyle : STRING;

TYPE ram_type IS ARRAY (2**addr_width-1 DOWNTO 0) OF UNSIGNED(data_width-1 DOWNTO 0);

CONSTANT data_hex : INTEGER := next_quad(data_width);

BEGIN

initialized_ram: PROCESS(clk)
	FILE tcf         : TEXT;
	VARIABLE first   : BOOLEAN := true;
	VARIABLE l	     : line;
	VARIABLE char    : character;
	VARIABLE adr     : UNSIGNED(15 DOWNTO 0);
	VARIABLE buf     : UNSIGNED(data_hex-1 DOWNTO 0);
	VARIABLE loc     : INTEGER RANGE 0 TO 2**addr_width+1;
   VARIABLE addr_d  : UNSIGNED(addr_width-1 DOWNTO 0);
   VARIABLE ram     : ram_type; ATTRIBUTE syn_ramstyle OF ram : VARIABLE IS ramstyle;
BEGIN
-- pragma translate_off
	IF  first AND init_file /= "none"  THEN
   	file_open(tcf, init_file, READ_MODE);
		l := NEW string'("initializing " & string'(init_file)); writeline(output, l);
      WHILE  NOT ENDFILE(tcf)  LOOP
         readline(tcf, l);
      	IF  l'length > 6  THEN
				hread(l, adr);	   -- read address
            loc := to_integer(adr);
            read(l, char);   -- skip : character
            read(l, char);   -- skip : character
            LOOP
					IF  l'length < (data_hex / 4)  THEN  EXIT; END IF;
            	hread(l, buf);  -- read 16 bit addresses = 4 hex digits
               ram(loc) := buf(data_width-1 DOWNTO 0);
               loc := loc + 1;
               IF  loc >= (2**addr_width)  THEN  EXIT;  END IF;
            END LOOP;
            IF  loc >= (2**addr_width)  THEN  EXIT;  END IF;
         END IF;
      END LOOP;
		file_close(tcf);
      first := false;
   ELSE
-- pragma translate_on
      IF  rising_edge(clk)   THEN
         IF  en = '1'  THEN
            addr_d := addr;
            IF  we = '1'  THEN
               ram(to_integer(addr)) := di;
            END IF;
         END IF;
      END IF;
      do <= ram(to_integer(addr_d));
-- pragma translate_off
   END IF;
-- pragma translate_on
END PROCESS initialized_ram;

END inference_model;

-- ---------------------------------------------------------------------
-- internal_dual_port_blockRAM:
-- Address latched prior to access, input data stored on rising clock edge.
-- During simulation initialized by an ascii-file of the following format:
-- addr   0        1        2        3
-- 0000 : 95930800 00000000 94F4471F 07000000
-- ---------------------------------------------------------------------

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;
USE STD.TEXTIO.ALL;
USE work.functions_pkg.ALL;

ENTITY internal_dpram IS GENERIC (
   data_width : INTEGER;
   addr_width : INTEGER;
   ramstyle   : STRING;
   init_file  : STRING
); PORT (
   clk   : IN    STD_LOGIC;
   ena   : IN    STD_LOGIC;
   wea   : IN    STD_LOGIC;
   addra : IN    UNSIGNED(addr_width-1 DOWNTO 0);
   dia   : IN    UNSIGNED(data_width-1 DOWNTO 0);
   doa   : OUT   UNSIGNED(data_width-1 DOWNTO 0);
   enb   : IN    STD_LOGIC;
   web   : IN    STD_LOGIC;
   addrb : IN    UNSIGNED(addr_width-1 DOWNTO 0);
   dib   : IN    UNSIGNED(data_width-1 DOWNTO 0);
   dob   : OUT   UNSIGNED(data_width-1 DOWNTO 0)
); END internal_dpram;

ARCHITECTURE inference_model OF internal_dpram IS

ATTRIBUTE syn_ramstyle : STRING;

TYPE ram_type IS ARRAY (2**addr_width-1 DOWNTO 0) OF UNSIGNED(data_width-1 DOWNTO 0);

CONSTANT data_hex : INTEGER := next_quad(data_width);

BEGIN

initialized_ram: PROCESS(clk)
	FILE tcf			  : TEXT;
	VARIABLE first	  : BOOLEAN := true;
	VARIABLE l	     : line;
	VARIABLE char    : character;
	VARIABLE adr     : UNSIGNED(15 DOWNTO 0);
	VARIABLE buf     : UNSIGNED(data_hex-1 DOWNTO 0);
	VARIABLE loc     : INTEGER RANGE 0 TO 2**addr_width+1;
   VARIABLE addra_d : UNSIGNED(addr_width-1 DOWNTO 0);
   VARIABLE addrb_d : UNSIGNED(addr_width-1 DOWNTO 0);
   VARIABLE ram     : ram_type; ATTRIBUTE syn_ramstyle OF ram : VARIABLE IS ramstyle;
BEGIN
-- pragma translate_off
	IF  first AND init_file /= "none"  THEN
   	file_open(tcf, init_file, READ_MODE);
		l := NEW string'("initializing " & string'(init_file)); writeline(output, l);
      WHILE  NOT ENDFILE(tcf)  LOOP
         readline(tcf, l);
      	IF  l'length > 6  THEN
				hread(l, adr);	   -- read address
            loc := to_integer(adr);
            read(l, char);   -- skip space
            read(l, char);   -- skip :
            LOOP
					IF  l'length < (data_hex / 4)  THEN  EXIT; END IF;
            	hread(l, buf);  -- read 16 bit addresses = 4 hex digits
               ram(loc) := buf(data_width-1 DOWNTO 0);
               loc := loc + 1;
               IF  loc >= (2**addr_width)  THEN  EXIT;  END IF;
            END LOOP;
            IF  loc >= (2**addr_width)  THEN  EXIT;  END IF;
         END IF;
      END LOOP;
		file_close(tcf);
      first := false;
   ELSE
-- pragma translate_on
      IF  rising_edge(clk)   THEN
         IF  ena = '1'  THEN
            addra_d := addra;
            IF  wea = '1'  THEN
               ram(to_integer(addra)) := dia;
            END IF;
         END IF;
         IF  enb = '1'  THEN
            addrb_d := addrb;
            IF  web = '1'  THEN
               ram(to_integer(addrb)) := dib;
            END IF;
         END IF;
      END IF;
      doa <= ram(to_integer(addra_d));
      dob <= ram(to_integer(addrb_d));
-- pragma translate_off
   END IF;
-- pragma translate_on
END PROCESS initialized_ram;

END inference_model;

-- ---------------------------------------------------------------------
-- external_ram: Input data is stored on falling WRITE edge
-- Initialized by an ascii-file of the following format:
-- addr          0        1        2        3
-- 0000 : 95930800 00000000 94F4471F 07000000
-- ---------------------------------------------------------------------

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;
USE STD.TEXTIO.ALL;
USE work.functions_pkg.ALL;

ENTITY external_ram IS
GENERIC (data_width  : INTEGER;
         addr_width  : INTEGER;
         init_file   : STRING := "none");
PORT (ce_n   : IN    STD_LOGIC;
      oe_n   : IN    STD_LOGIC;
      we_n   : IN    STD_LOGIC;
      addr   : IN    UNSIGNED(addr_width-1 DOWNTO 0);
      data   : INOUT UNSIGNED(data_width-1 DOWNTO 0));
END external_ram;

ARCHITECTURE sim_model OF external_ram IS

TYPE ram_type IS ARRAY (2**addr_width-1 DOWNTO 0) OF UNSIGNED(data_width-1 DOWNTO 0);

CONSTANT data_hex : INTEGER := next_quad(data_width);
SIGNAL addr_i     : UNSIGNED(addr_width-1 DOWNTO 0);
SIGNAL ce         : STD_LOGIC;
SIGNAL din        : UNSIGNED(data_width-1 DOWNTO 0);

BEGIN

addr_i <= addr AFTER 1 ns; -- delay for the sake of simulation
din <= data AFTER 1 ns;    -- delay for the sake of simulation
ce <= NOT ce_n AFTER 1 ns;

initialized_ram: PROCESS(ce, oe_n, we_n, addr_i, din)
	FILE tcf			: TEXT;
	VARIABLE first	: BOOLEAN := true;
	VARIABLE l	   : line;
	VARIABLE char  : character;
	VARIABLE adr   : UNSIGNED(15 DOWNTO 0);
	VARIABLE buf   : UNSIGNED(data_hex-1 DOWNTO 0);
	VARIABLE loc   : INTEGER RANGE 0 TO 2**addr_width+1;
   VARIABLE ram   : ram_type;
BEGIN
-- pragma translate_off
	IF  first AND init_file /= "none"  THEN
   	file_open(tcf, init_file, READ_MODE);
		l := NEW string'("initializing " & string'(init_file)); writeline(output, l);
      WHILE  NOT ENDFILE(tcf)  LOOP
         readline(tcf, l);
      	IF  l'length > 6  THEN
				hread(l, adr);	   -- read address
            loc := to_integer(adr);
            read(l, char);   -- skip space
            read(l, char);   -- skip :
            LOOP
					IF  l'length < (data_hex / 4)  THEN  EXIT; END IF;
            	hread(l, buf);  -- read 16 bit addresses = 4 hex digits
               ram(loc) := buf(data_width-1 DOWNTO 0);
               loc := loc + 1;
--               l := NEW string'("."); writeline(output, l);
               IF  loc >= (2**addr_width)  THEN  EXIT;  END IF;
            END LOOP;
            IF  loc >= (2**addr_width)  THEN  EXIT;  END IF;
         END IF;
      END LOOP;
		file_close(tcf);
      first := false;
   ELSE
-- pragma translate_on
      data <= (OTHERS => 'Z');
      IF  ce = '1'  THEN
         IF  rising_edge(we_n)  THEN
            ram(to_integer(addr_i)) := din;
         END IF;
         IF  we_n = '1' AND oe_n = '0'  THEN
            data <= ram(to_integer(addr_i));
         END IF;
      END IF;
-- pragma translate_off
   END IF;
-- pragma translate_on
END PROCESS initialized_ram;

END sim_model;

-- ---------------------------------------------------------------------
-- DONKA - Hardware & VHDL Design
-- Monoflop
-- Target: independent
-- (c) 06/06 Thomas Wendt, DONKA - Hardware & VHDL Design
-- V1.0 10.08.2006 Erstellung
-- V1.1 21.09.2006 durch resetCnt nun retriggerbar
-- V2.0 27.02.2009 change to synchronous design
-- ---------------------------------------------------------------------

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;
USE work.functions_pkg.async_reset;

ENTITY monoflop IS
GENERIC (ptime : integer := 10000000);  -- Pulsdauer in clk Perioden
PORT (clk      : IN STD_LOGIC;          -- Input Clock
      reset    : IN STD_LOGIC;          -- Async. Reset
      inrt     : IN STD_LOGIC;          -- retriggerbarer Input
      pout     : OUT STD_LOGIC);        -- Output Pulse
END monoflop;

ARCHITECTURE behavior OF monoflop IS

SIGNAL pcnt      : INTEGER RANGE 1 TO ptime;
SIGNAL inrt_L    : STD_LOGIC;
SIGNAL inrt_LL   : STD_LOGIC;
SIGNAL inrt_P    : STD_LOGIC;

BEGIN

p_trg_pulse : PROCESS(clk, reset)
BEGIN
   IF  reset = '1' AND async_reset  THEN
      inrt_L  <= '0';
      inrt_LL <= '0';
      inrt_P  <= '0';
   ELSIF rising_edge(clk) THEN
      inrt_L  <= inrt;
      inrt_LL <= inrt_L;
      inrt_P  <= inrt_L XOR inrt_LL;
      IF  reset = '1' AND NOT async_reset  THEN
         inrt_L  <= '0';
         inrt_LL <= '0';
         inrt_P  <= '0';
      END IF;
   END IF;
END PROCESS;

p_cnt : PROCESS(clk, reset)
BEGIN
   IF  reset = '1' AND async_reset  THEN
      pcnt <= 1;
      pout <= '1';
   ELSIF rising_edge(clk) THEN
      IF inrt_P = '1' THEN
         pcnt  <= 1;
         pout  <= '1';
      ELSIF pcnt = ptime THEN
         pout  <= '0';
      ELSE
         pcnt <= pcnt + 1;
      END IF;
      IF  reset = '1' AND NOT async_reset  THEN
         pcnt <= 1;
         pout <= '1';
      END IF;
   END IF;
END PROCESS;

END behavior;

-- ---------------------------------------------------------------------
-- FIFO with generic WIDTH and DEPTH
-- derived from fifo_af.vhd of the RTU-ASIC project.
-- ---------------------------------------------------------------------

LIBRARY IEEE;
USE ieee.std_logic_1164.all;
USE ieee.NUMERIC_STD.all;
USE work.functions_pkg.async_reset;

ENTITY fifo IS GENERIC (
   width    : NATURAL;
	depth    : NATURAL;
   ramstyle : STRING
); PORT (
   reset    : IN  STD_LOGIC;
   clk      : IN  STD_LOGIC;
   push     : IN  STD_LOGIC;
   pop      : IN  STD_LOGIC;
   empty    : OUT STD_LOGIC;
   full     : OUT STD_LOGIC;
   din      : IN  UNSIGNED(width-1 DOWNTO 0);
   dout     : OUT UNSIGNED(width-1 DOWNTO 0)
); END fifo;

ARCHITECTURE rtl OF fifo IS

ATTRIBUTE syn_ramstyle : STRING;

TYPE fifoarr IS ARRAY (NATURAL RANGE 0 TO depth-1) OF UNSIGNED(width-1 DOWNTO 0);

SIGNAL fifomem   : fifoarr; ATTRIBUTE syn_ramstyle OF fifomem : SIGNAL IS ramstyle;
SIGNAL write_ptr : NATURAL RANGE 0 TO depth-1;
SIGNAL read_ptr  : NATURAL RANGE 0 TO depth-1;
SIGNAL read_addr : NATURAL RANGE 0 TO depth-1;
SIGNAL cnt       : NATURAL RANGE 0 TO depth;

BEGIN

dout  <= fifomem(read_addr);
full  <= '1' WHEN cnt = depth  ELSE '0';
empty <= '1' WHEN cnt = 0      ELSE '0';

make_addr_proc : PROCESS (read_ptr, cnt)  -- when the FIFO is empty
BEGIN                                     -- the last valid data will be accessed
	IF  cnt /= 0  THEN
		read_addr <= read_ptr;
	ELSE
		IF  read_ptr = 0  THEN
			read_addr <= depth - 1;
		ELSE
			read_addr <= read_ptr - 1;
		END IF;
	END IF;
END PROCESS make_addr_proc;

fifo_reg_proc : PROCESS(clk, reset)
BEGIN
   IF  reset = '1' AND async_reset  THEN
		cnt <= 0;
		write_ptr <= 0;
		read_ptr  <= 0;
	ELSIF  rising_edge(clk)  THEN
		IF  push = '1' AND cnt /= depth  THEN
			fifomem(write_ptr) <= din;
			cnt <= cnt + 1;
			IF  write_ptr = depth - 1  THEN
				write_ptr <= 0;
			ELSE
				write_ptr <= write_ptr + 1;
			END IF;
		END IF;
		IF  pop = '1' AND cnt /= 0  THEN
			cnt <= cnt - 1;
			IF  read_ptr = depth - 1  THEN
				read_ptr <= 0;
			ELSE
				read_ptr <= read_ptr + 1;
			END IF;
			IF  push = '1' AND cnt /= depth  THEN
				cnt <= cnt;
			END IF;
		END IF;
		IF  reset = '1' AND NOT async_reset  THEN
			cnt <= 0;
			write_ptr <= 0;
			read_ptr  <= 0;
		END IF;
	END IF;
END PROCESS fifo_reg_proc;

END rtl;
