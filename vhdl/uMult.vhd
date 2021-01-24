-- ---------------------------------------------------------------------
-- @file : uMult.vhd
-- ---------------------------------------------------------------------
--
-- Last change: KS 24.01.2021 19:50:29
-- Project : microCore
-- Language : VHDL-2008
-- Last check in : $Rev: 559 $ $Date:: 2020-06-13 #$
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
-- @brief: Definition of the multiplier.
--         Here fpga specific multiplier IP can be included.
--
-- Version Author   Date       Changes
--           ks    8-Jun-2020  initial version
-- ---------------------------------------------------------------------
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;
USE work.architecture_pkg.ALL;

ENTITY uMult IS PORT (
   multiplicand : IN  STD_LOGIC_VECTOR(data_width DOWNTO 0);
   multiplier   : IN  STD_LOGIC_VECTOR(data_width DOWNTO 0);
   product      : OUT STD_LOGIC_VECTOR(data_width*2+1 DOWNTO 0)
); END uMult;

ARCHITECTURE inference OF uMult IS

BEGIN

product <= STD_LOGIC_VECTOR (SIGNED(multiplicand) * SIGNED(multiplier));

END inference;
