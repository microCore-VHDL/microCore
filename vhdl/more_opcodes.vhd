-----------------------------------------------------------------
-- MORE_OPCODES.VHD
-----------------------------------------------------------------
--
-- Last change: KS 23.01.2020 17:13:19
--
-- Some of these are uCore 1.xx opcodes, which have not yet been
-- ported to uCore 2.xx
-----------------------------------------------------------------

-----------------------------------------------------------------
-- indexed addressing: effective addr = TOR+TOS
-----------------------------------------------------------------
-- op_TOR_LD      Op: ++ld   ( index -- n addr )   don't
op_TOR_LD      Op: ++ld   ( index -- n addr )   don't
            Macro: ++@    ( index -- n )        ?comp T ++ld drop H ;
      WHEN op_TOR_LD => add_x <= r.tos;
                        add_y <= r.tor;
                        cin <= '0';
                        push_stack;
                        r_in.tos <= sum;
                        mem_addr <= sum(data_addr_width-1 DOWNTO 0);
                        mem_en <= '1';
                        set_opcode(op_MEM2NOS);

-- op_TOR_ST      Op: ++st   ( n index -- addr )   don't
op_TOR_ST      Op: ++st   ( n index -- addr )   don't
            Macro: ++!    ( n index -- )        ?comp T ++st drop H ;
      WHEN op_TOR_ST => add_x <= r.tos;
                        add_y <= r.tor;
                        cin <= '0';
                        pop_stack;
                        r_in.tos <= sum;
                        mem_addr <= sum(data_addr_width-1 DOWNTO 0);
                        mem_wdata <= r.nos;
                        mem_en <= '1';
                        mem_wr <= '1';

------------------------------------------------------------------------------
-- shifting - dshift, dashift bitwise
------------------------------------------------------------------------------

op_SL          Op: 2*      ( u1 -- u2 )       2*

      WHEN op_SL    => r_in.tos <= r.tos(r.tos'high-1 DOWNTO 0) & '0';  -- logical shift left: 0 goes into 0th bit pos.
                       r_in.status(s_c) <= r.tos(r.tos'high);

op_ASR         Op: 2/      ( n1 -- n2 )       2/

      WHEN op_ASR   => r_in.tos <= r.tos(r.tos'high) & r.tos(r.tos'high DOWNTO 1); -- preserve the sign.
                       r_in.status(s_c) <= r.tos(0);

op_LSR         Op: u2/     ( u1 -- u2 )       u2/

      WHEN op_LSR   => r_in.tos <= '0' & r.tos(r.tos'high DOWNTO 1);    -- LSR: 0 goes into MSB position.
                       r_in.status(s_c) <= r.tos(0);

op_DASHIFT     Op: dashift ( d n -- d' )      don't
            Macro: d2/     ( d -- d' )        ?comp -1 lit, T dashift H ;

      WHEN op_DASHIFT => IF  extended  THEN
                           IF  tos_zero = '1'  THEN
                              pop_stack;
                              r_in.status(s_c) <= '0';
                           ELSE -- r.tos /= 0
                             IF  r.tos(r.tos'high) = '1'  THEN -- shift right
                                IF  r.tos = -1  THEN
                                   pop_stack;
                                   r_in.tos <= r.nos(r.nos'high) & r.nos(r.nos'high DOWNTO 1);
                                   r_in.nos <= r.nos(0) & ds_rdata(ds_rdata'high DOWNTO 1);
                                   r_in.status(s_c) <= ds_rdata(0);
                                ELSE
                                   r_in.nos <= r.nos(r.nos'high) & r.nos(r.nos'high DOWNTO 1);
                                   ds_wdata <= r.nos(0) & ds_rdata(ds_rdata'high DOWNTO 1);
                                   ds_wr <= '1';
                                   r_in.tos <= r.tos + 1;
                                   paddr <= prog_addr;
                                END IF;
                             ELSE                              -- shift left
                                IF  r.tos = 1  THEN
                                   pop_stack;
                                   r_in.tos <= r.nos(r.nos'high - 1 DOWNTO 0) & ds_rdata(ds_rdata'high);
                                   r_in.nos <= ds_rdata(ds_rdata'high - 1 DOWNTO 0) & '0';
                                   r_in.status(s_c) <= r.nos(r.nos'high);
                                ELSE
                                   r_in.nos <= r.nos(r.nos'high - 1 DOWNTO 0) & '0';
                                   ds_wdata <= ds_rdata(ds_rdata'high - 1 DOWNTO 0) & '0';
                                   ds_wr <= '1';
                                   r_in.tos <= r.tos - 1;
                                   paddr <= prog_addr;
                                END IF;
                             END IF;
                           END IF;
                        END IF;

op_DSHIFT      Op: dshift  ( ud n -- ud' )    don't
            Macro: d2*     ( ud -- ud' )      ?comp  1 lit, T dshift H ;
            Macro: ud2/    ( ud -- ud' )      ?comp -1 lit, T dshift H ;

      WHEN op_DSHIFT => IF  extended  THEN
                           IF  tos_zero = '1'  THEN
                              pop_stack;
                              r_in.status(s_c) <= '0';
                           ELSE -- r.tos /= 0
                             IF  r.tos(r.tos'high) = '1'  THEN -- shift right
                                IF  r.tos = -1  THEN
                                   pop_stack;
                                   r_in.tos <= '0' & r.nos(r.nos'high DOWNTO 1);
                                   r_in.nos <= r.nos(0) & ds_rdata(ds_rdata'high DOWNTO 1);
                                   r_in.status(s_c) <= ds_rdata(0);
                                ELSE
                                   r_in.nos <= '0' & r.nos(r.nos'high DOWNTO 1);
                                   ds_wdata <= r.nos(0) & ds_rdata(ds_rdata'high DOWNTO 1);
                                   ds_wr <= '1';
                                   r_in.tos <= r.tos + 1;
                                   paddr <= prog_addr;
                                END IF;
                             ELSE                              -- shift left
                                IF  r.tos = 1  THEN
                                   pop_stack;
                                   r_in.tos <= r.nos(r.nos'high - 1 DOWNTO 0) & ds_rdata(ds_rdata'high);
                                   r_in.nos <= ds_rdata(ds_rdata'high - 1 DOWNTO 0) & '0';
                                   r_in.status(s_c) <= r.nos(r.nos'high);
                                ELSE
                                   r_in.nos <= r.nos(r.nos'high - 1 DOWNTO 0) & ds_rdata(ds_rdata'high);
                                   ds_wdata <= ds_rdata(ds_rdata'high - 1 DOWNTO 0) & '0';
                                   ds_wr <= '1';
                                   r_in.tos <= r.tos - 1;
                                   paddr <= prog_addr;
                                END IF;
                             END IF;
                           END IF;
                        END IF;

------------------------------------------------------------------------------
-- byte manipulation -- uCore_1.xx code
------------------------------------------------------------------------------

--op_PACK        Op: pack    ( c u -- u' )      don't
      WHEN op_PACK  => pop_stack;
                       r_in.tos <= r.tos(r.tos'high-8 DOWNTO 0) & r.nos(7 DOWNTO 0);

--op_UNPACK      Op: unpack  ( u -- c u' )      don't
      WHEN op_UNPACK => push_stack;
                       r_in.nos <= (slice('0', data_width-8) & r.tos(7 DOWNTO 0));
                       r_in.tos <= "00000000" & r.tos(r.tos'high DOWNTO 8);

-----------------------------------------------------------------
-- branches -- uCore_1.xx code
-----------------------------------------------------------------

op_FWRD        Op: >FOR    ( n -- R: n )      don't
      WHEN op_FWRD  => push_rstack;
                       pop_stack;
                       s_in.fwrd <= r.tos(r.tos'high);
                       IF  r.tos = 0  THEN
                          r_in.tor <= r.tos;
                          paddr <= r.pc + 1;
                       ELSIF  r.tos(r.tos'high) = '0'  THEN
                          r_in.tor <= r.tos - 1;
                       ELSE
                          r_in.tor <= NOT r.tos;
                       END IF;

op_BACK        Op: <NEXT   ( -- rdrop )       don't
      WHEN op_BACK  => IF  r.tor = 0  THEN
                          pop_rstack;
                          s_in.fwrd <= '1';
                       ELSE
                          r_in.tor <= r.tor - 1;
                          paddr <= r.pc - 2;
                       END IF;

      WHEN op_ZEXIT  => pop_stack;
                        IF  s_z = '1'  THEN
                           pop_rstack;
                           paddr <= r.tor(prog_addr_width-1 DOWNTO 0);
                        END IF;

--op_QZERO      Brn: ?-branch   ( addr ?f -- )  don't  \      ?dup IF
      WHEN op_QZERO  => conditional(s_zero);
                        IF  s_z = '1'  THEN
                           r_in.inst <= op_DROP;
                           r_in.pc <= paddr;
                        END IF;

--op_SIGN       Brn: s-branch   ( addr f -- )   don't  \     0< 0= IF
      WHEN op_SIGN   => conditional(s_neg);
                        r_in.inst <= op_DROP;
                        r_in.pc <= paddr;

--op_NSIGN      Brn: ns-branch  ( addr f -- )   don't  \        0< IF
      WHEN op_NSIGN  => conditional(NOT s_neg);
                        r_in.inst <= op_DROP;
                        r_in.pc <= paddr;

--op_NZERO      Brn: 0<>branch  ( addr f -- )   don't  \        0= IF
      WHEN op_NZERO  => conditional(NOT s_z);
                        r_in.inst <= op_DROP;
                        r_in.pc <= paddr;

--op_NOVFL      Brn: no-branch  ( addr -- )     don't  \ ovfl?     IF
      WHEN op_NOVFL  => conditional(NOT s.ovfl);

--op_NCARRY     Brn: nc-branch  ( addr -- )     don't  \ carry?    IF
      WHEN op_NCARRY => conditional(NOT s.c);

------------------------------------------------------------------------------
-- byte memory access for C -- uCore_1.xx code
------------------------------------------------------------------------------

CONSTANT with_bytes        : BOOLEAN := false ; -- true with dmem read-modify-write 8b and 16b instructions
--WITH_BYTES [IF]
--op_cLD         Op: cLD    ( caddr -- u8b  caddr )  don't
--op_wLD         Op: wLD    ( caddr -- u16b caddr )  don't
--op_iLD         Op: iLD    ( caddr -- n    caddr )  don't
--op_cST         Op: cST    ( 8b  caddr -- addr )    don't
--op_wST         Op: wST    ( 16b caddr -- addr )    don't
--op_iST         Op: iST    ( n   caddr -- addr )    don't
--            Macro: c@     ( caddr -- u8b )    ?comp T cLD drop H ;
--            Macro: w@     ( caddr -- u16b )   ?comp T wLD drop H ;
--            Macro: i@     ( caddr -- n )      ?comp T iLD drop H ;
--            Macro: c!     ( 8b  caddr -- )    ?comp T cST drop H ;
--            Macro: w!     ( 16b caddr -- )    ?comp T wST drop H ;
--            Macro: i!     ( n   caddr -- )    ?comp T iST drop H ;
--op_SIGNED      Op: signed ( 16b -- n )        don't
--            Macro: >byte  ( addr -- caddr )   ?comp T 2* 2* H ;
--            Macro: byte>  ( caddr -- addr )   ?comp T u2/ u2/ H ;
--[THEN]

-- byte store instructions, cST and wST are 2 cycle read-modify-write
      WHEN op_cST => IF  WITH_BYTES  THEN
                        mem_en <= '1';
                        mem_addr <= "00" & r.tos(r.tos'high DOWNTO 2);
                        r_in.tos <= "00" & r.tos(r.tos'high DOWNTO 2);
                        CASE  r.tos(1 DOWNTO 0)  IS  -- little endian byte order
                        WHEN "00"   => r_in.nos <= mem_rdata(dw-1 DOWNTO    8) & r.nos(7 DOWNTO 0);
                        WHEN "01"   => r_in.nos <= mem_rdata(dw-1 DOWNTO   16) & r.nos(7 DOWNTO 0) & mem_rdata(   7 DOWNTO 0);
                        WHEN "10"   => r_in.nos <= mem_rdata(dw-1 DOWNTO dw-8) & r.nos(7 DOWNTO 0) & mem_rdata(  15 DOWNTO 0);
                        WHEN OTHERS => r_in.nos <=                              r.nos(7 DOWNTO 0) & mem_rdata(dw-9 DOWNTO 0);
                        END CASE;
                        r_in.inst <= op_STORE;
                        r_in.pc <= paddr;
                     END IF;

      WHEN op_wST => IF  WITH_BYTES  THEN
                        IF  r.tos(0) = '0'  THEN  -- valid byte address
                           mem_en <= '1';
                           mem_addr <= "00" & r.tos(r.tos'high DOWNTO 2);
                           r_in.tos <= "00" & r.tos(r.tos'high DOWNTO 2);
                           IF  r.tos(1) = '0'  THEN
                              r_in.nos <= mem_rdata(dw-1 DOWNTO dw-16) & r.nos(15 DOWNTO 0);
                           ELSE
                              r_in.nos <= r.nos(15 DOWNTO 0) & mem_rdata(15 DOWNTO 0);
                           END IF;
                           r_in.inst <= op_STORE;
                           r_in.pc <= paddr;
                        ELSE                  -- invalid byte address
                           r_in.tos <= r.tos(r.tos'high DOWNTO 1) & '0';
                           call_trap(op_ADDR(4 DOWNTO 0));
                        END IF;
                     END IF;

      WHEN op_iST => IF  WITH_BYTES  THEN
                        IF  r.tos(1 DOWNTO 0) = "00"  THEN -- valid byte address?
                           pop_stack;
                           r_in.tos <= "00" & r.tos(r.tos'high DOWNTO 2);
                           mem_addr <= "00" & r.tos(r.tos'high DOWNTO 2);
                           mem_en <= '1';
                           mem_wr <= '1';
                           mem_wdata <= r.nos;
                        ELSE                           -- invalid byte address
                           r_in.tos <= r.tos(r.tos'high DOWNTO 2) & "00";
                           call_trap(op_ADDR(4 DOWNTO 0));
                        END IF;
                     END IF;

-- byte load instructions
      WHEN op_cLD => IF  WITH_BYTES  THEN
                        push_stack;
                        mem_en <= '1';
                        mem_addr <= "00" & r.tos(r.tos'high DOWNTO 2);
                        CASE r.tos(1 DOWNTO 0) IS -- little endian byte order
                        WHEN "00"   => r_in.nos <= slice('0', data_width-8) & mem_rdata(   7 DOWNTO  0);
                        WHEN "01"   => r_in.nos <= slice('0', data_width-8) & mem_rdata(  15 DOWNTO  8);
                        WHEN "10"   => r_in.nos <= slice('0', data_width-8) & mem_rdata(  23 DOWNTO 16);
                        WHEN OTHERS => r_in.nos <= slice('0', data_width-8) & mem_rdata(dw-1 DOWNTO 24);
                        END CASE;
                        s_in.word <= '0';
                     END IF;

      WHEN op_wLD => IF  WITH_BYTES  THEN
                        IF  r.tos(0) = '0'  THEN  -- valid byte address
                           push_stack;
                           mem_en <= '1';
                           mem_addr <= "00" & r.tos(r.tos'high DOWNTO 2);
                           IF  r.tos(1) = '0'  THEN
                              r_in.nos <= slice('0', data_width-16) & mem_rdata(15 DOWNTO  0);
                           ELSE
                              r_in.nos <= slice('0', data_width-16) & mem_rdata(dw-1 DOWNTO 16);
                           END IF;
                           s_in.word <= '1';
                        ELSE                  -- invalid byte address
                           r_in.tos <= r.tos(r.tos'high DOWNTO 1) & '0';
                           call_trap(op_ADDR(4 DOWNTO 0));
                        END IF;
                     END IF;

      WHEN op_iLD => IF  WITH_BYTES  THEN
                        IF  r.tos(1 DOWNTO 0) = "00"  THEN -- valid byte address?
                           push_stack;
                           mem_en <= '1';
                           mem_addr <= "00" & r.tos(r.tos'high DOWNTO 2);
                           r_in.nos <= mem_rdata;
                        ELSE                           -- invalid byte address
                           r_in.tos <= r.tos(r.tos'high DOWNTO 2) & "00";
                           call_trap(op_ADDR(4 DOWNTO 0));
                        END IF;
                     END IF;

-- additional status bit needed to remember word/byte access
-- s.word    : STD_LOGIC; -- previous wLD='1', cLD='0'
      WHEN op_SIGNED => -- sign extension after cLD or wLD
                       IF  WITH_BYTES  THEN
                          IF  s.word = '0'  THEN
                             r_in.tos <= slice(r.tos( 7), 24) & r.tos( 7 DOWNTO 0);
                          ELSE
                             r_in.tos <= slice(r.tos(15), 16) & r.tos(15 DOWNTO 0);
                          END IF;
                       END IF;

WITH_BYTES [IF]
H op_ADDR  #usrmask and T Constant #asr    \ byte address error w@/! i@/!
Variable foo   : addrtrap  ( paddr -- )    1 foo +!  BRANCH ;
#asr   TRAP: asr   ( -- ) r> 1- addrtrap         ;  \ misaligned address trap
[THEN]

: test_byteaddr  ( -- )
[ WITH_BYTES ] [IF]
   0 Foo !   Location >byte >r
   $11 r@ c!  $22 r@ 1+ c!  $33 r@ 2 + c!   $44 r@ 3 + c!
   Location @ $44332211 -       IF  $D0 finis  THEN
   r@ i@      $44332211 -       IF  $D1 finis  THEN
   r@ 1+ i@   $44332211 -       IF  $D2 finis  THEN -- addrtrap
   r@ w@      $2211 -           IF  $D3 finis  THEN
   r@ 2 + w@  $4433 -           IF  $D4 finis  THEN
   r@ 1+ w@   $2211 -           IF  $D5 finis  THEN -- addrtrap
   $11223388 r@ 1+ i!                               -- addrtrap
   r@ c@ signed $FFFFFF88 -     IF  $D6 finis  THEN
   r@ 1+ c@ $33 -               IF  $D7 finis  THEN
   r@ 2 + c@ signed $22 -       IF  $D8 finis  THEN
   r@ 3 + c@ $11 -              IF  $D9 finis  THEN
   r@ w@ $3388 -                IF  $DA finis  THEN
   r@ 2 + w@ $1122 -            IF  $DB finis  THEN
   $8888 r@ 1+ w!                                   -- addrtrap
   $4444 r@ 2 + w!
   r@ w@ signed $FFFF8888 -     IF  $DC finis  THEN
   r@ 2 + w@ signed $4444 -     IF  $DD finis  THEN
   r> byte> Location -          IF  $DE finis  THEN
[THEN]
;
