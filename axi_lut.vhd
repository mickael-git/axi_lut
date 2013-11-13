------------------------------------------------------------------------
--  axi_lut.vhd
--  apply a LUT on video_in input, latency = 1 clk
--
--  Copyright (C) 2013 M.FORET
--
--  This program is free software: you can redistribute it and/or
--  modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation, either version
--  2 of the License, or (at your option) any later version.
------------------------------------------------------------------------

-- ToDo : to manage 2 areas in memory so that 1 is used for live
-- processing and other 1 can be updated, switch of the 2 areas is
-- synchronized by start of frame

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.pkg_axi_lut.all;  -- components

use work.axi3m_pkg.all;  -- axi records

-- for RAM
Library UNISIM;
use UNISIM.vcomponents.all;
library UNIMACRO;
use unimacro.Vcomponents.all;

entity axi_lut is
  generic (
    DATA_WIDTH_OUT   : positive := 8  -- data width output (1 to 16)
  );
  port (
    -- ========= AXI
    s_axi_aclk     : in  std_logic;
    --
    s_axi_areset_n : in  std_logic;

    -- write interface
    s_axi_wi : in  axi3m_write_out_r;
    s_axi_wo : out axi3m_write_in_r;

    -- read interface
    s_axi_ri : in  axi3m_read_out_r;
    s_axi_ro : out axi3m_read_in_r;

    -- ========= video interface
    video_clk     : in  std_logic;
    video_in      : in  std_logic_vector(11 downto 0);
    lut_out       : out std_logic_vector(DATA_WIDTH_OUT-1 downto 0)
  );
end entity;

architecture rtl of axi_lut is

function log2(val: natural) return natural is
  variable res : natural;
begin
  for i in 30 downto 0 loop
    if (val > (2**i)) then
      res := i;
      exit;
    end if;
  end loop;
  return (res + 1);
end function log2;

constant DATA_WIDTH_IN   : natural := video_in'length;
-- compute address width of memory = DATA_WIDTH_IN + log2(DATA_WIDTH_OUT) - ln2(32)
constant ADDR_WIDTH      : natural := DATA_WIDTH_IN + log2(DATA_WIDTH_OUT) - 5;

constant gnd_din         : std_logic_vector(DATA_WIDTH_OUT-1 downto 0) := (others=>'0');
constant gnd_8b          : std_logic_vector( 7 downto 0) := (others=>'0');

signal mem_addr      : std_logic_vector(ADDR_WIDTH-1 downto 0);
signal mem_we        : std_logic_vector( 3 downto 0);
signal mem_din       : std_logic_vector(31 downto 0);
signal mem_dout      : std_logic_vector(31 downto 0);

signal tmp8b1        : std_logic_vector( 7 downto 0);
signal tmp8b2        : std_logic_vector( 7 downto 0);

-- signal used only in the case data_width_out > 8
signal dia0          : std_logic_vector(15 downto 0);
signal dia1          : std_logic_vector(15 downto 0);
signal doa0          : std_logic_vector(15 downto 0);
signal doa1          : std_logic_vector(15 downto 0);

begin


axi_to_mem0 : axi_to_memory
  generic map (
    ADDR_WIDTH    => ADDR_WIDTH
  )
  port map (
    s_axi_aclk     => s_axi_aclk,

    s_axi_areset_n => s_axi_areset_n,

    s_axi_wi       => s_axi_wi,
    s_axi_wo       => s_axi_wo,

    s_axi_ri       => s_axi_ri,
    s_axi_ro       => s_axi_ro,

    mem_addr       => mem_addr,
    mem_we         => mem_we  ,
    mem_din        => mem_din ,
    mem_dout       => mem_dout
  );


-- if DATA_WIDTH_OUT < 9 => 1 RAM36B : porta=32 bits, portb=8bits
-- if DATA_WIDTH_OUT > 8 => 2 RAM36B : porta=16 bits, portb=8bits

gen0 :
if (DATA_WIDTH_OUT > 0) and (DATA_WIDTH_OUT < 9) generate

inst_ram0 : BRAM_TDP_MACRO
  generic map (
    BRAM_SIZE => "36Kb", -- Target BRAM, "18Kb" or "36Kb"
    DEVICE => "7SERIES", -- Target Device: "VIRTEX5", "VIRTEX6", "7SERIES", "SPARTAN6"
    DOA_REG => 0, -- Optional port A output register (0 or 1)
    DOB_REG => 0, -- Optional port B output register (0 or 1)
    INIT_A => X"000000000", -- Initial values on A output port
    INIT_B => X"000000000", -- Initial values on B output port
    INIT_FILE => "NONE",
    READ_WIDTH_A => 32,  -- Valid values are 1-36 (19-36 only valid when BRAM_SIZE="36Kb")
    READ_WIDTH_B => 8,  -- Valid values are 1-36 (19-36 only valid when BRAM_SIZE="36Kb")
    SIM_COLLISION_CHECK => "ALL", -- Collision check enable "ALL", "WARNING_ONLY", "GENERATE_X_ONLY" or "NONE"
    SRVAL_A => X"000000000",  -- Set/Reset value for A port output
    SRVAL_B => X"000000000",  -- Set/Reset value for B port output
    WRITE_MODE_A => "WRITE_FIRST", -- "WRITE_FIRST", "READ_FIRST" or "NO_CHANGE"
    WRITE_MODE_B => "WRITE_FIRST", -- "WRITE_FIRST", "READ_FIRST" or "NO_CHANGE"
    WRITE_WIDTH_A => 32, -- Valid values are 1-36 (19-36 only valid when BRAM_SIZE="36Kb")
    WRITE_WIDTH_B => 8  -- Valid values are 1-36 (19-36 only valid when BRAM_SIZE="36Kb")
  )
  port map (
    CLKA   => s_axi_aclk , -- 1-bit input port-A clock
    DIA    => mem_din    , -- Input port-A data, width defined by WRITE_WIDTH_A parameter
    DOA    => mem_dout   , -- Output port-A data, width defined by READ_WIDTH_A parameter
    ADDRA  => mem_addr   , -- Input port-A address, width defined by Port A depth
    ENA    => '1'        , -- 1-bit input port-A enable
    REGCEA => '1'        , -- 1-bit input port-A output register enable
    RSTA   => '0'        , -- 1-bit input port-A reset
    WEA    => mem_we     , -- Input port-A write enable, width defined by Port A depth

    CLKB   => video_clk  , -- 1-bit input port-B clock
    DIB    => gnd_8b     , -- Input port-B data, width defined by WRITE_WIDTH_B parameter
    DOB    => tmp8b1     , -- Output port-B data, width defined by READ_WIDTH_B parameter
    ADDRB  => video_in   , -- Input port-B address, width defined by Port B depth
    ENB    => '1'        , -- 1-bit input port-B enable
    REGCEB => '1'        , -- 1-bit input port-B output register enable
    RSTB   => '0'        , -- 1-bit input port-B reset
    WEB    => "0"         -- Input port-B write enable, width defined by Port B depth
  );

lut_out <= tmp8b1(lut_out'length-1 downto 0);

end generate;

gen1 :
if (DATA_WIDTH_OUT > 8) and (DATA_WIDTH_OUT <17) generate

-- we split data as each memory contains 8bits of the 16 bits output
dia0 <= mem_din(23 downto 16) & mem_din( 7 downto 0);
dia1 <= mem_din(31 downto 24) & mem_din(15 downto 8);

mem_dout( 7 downto 0)  <= doa0( 7 downto 0);
mem_dout(15 downto 8)  <= doa1( 7 downto 0);
mem_dout(23 downto 16) <= doa0(15 downto 8);
mem_dout(31 downto 24) <= doa1(15 downto 8);

inst_ram0 : BRAM_TDP_MACRO
  generic map (
    BRAM_SIZE => "36Kb", -- Target BRAM, "18Kb" or "36Kb"
    DEVICE => "7SERIES", -- Target Device: "VIRTEX5", "VIRTEX6", "7SERIES", "SPARTAN6"
    DOA_REG => 0, -- Optional port A output register (0 or 1)
    DOB_REG => 0, -- Optional port B output register (0 or 1)
    INIT_A => X"000000000", -- Initial values on A output port
    INIT_B => X"000000000", -- Initial values on B output port
    INIT_FILE => "NONE",
    READ_WIDTH_A => 16,  -- Valid values are 1-36 (19-36 only valid when BRAM_SIZE="36Kb")
    READ_WIDTH_B => 8,  -- Valid values are 1-36 (19-36 only valid when BRAM_SIZE="36Kb")
    SIM_COLLISION_CHECK => "ALL", -- Collision check enable "ALL", "WARNING_ONLY", "GENERATE_X_ONLY" or "NONE"
    SRVAL_A => X"000000000",  -- Set/Reset value for A port output
    SRVAL_B => X"000000000",  -- Set/Reset value for B port output
    WRITE_MODE_A => "WRITE_FIRST", -- "WRITE_FIRST", "READ_FIRST" or "NO_CHANGE"
    WRITE_MODE_B => "WRITE_FIRST", -- "WRITE_FIRST", "READ_FIRST" or "NO_CHANGE"
    WRITE_WIDTH_A => 16, -- Valid values are 1-36 (19-36 only valid when BRAM_SIZE="36Kb")
    WRITE_WIDTH_B => 8  -- Valid values are 1-36 (19-36 only valid when BRAM_SIZE="36Kb")
  )
  port map (
    CLKA   => s_axi_aclk, -- 1-bit input port-A clock
    DIA    => dia0, -- Input port-A data, width defined by WRITE_WIDTH_A parameter
    DOA    => doa0, -- Output port-A data, width defined by READ_WIDTH_A parameter
    ADDRA  => mem_addr  , -- Input port-A address, width defined by Port A depth
    ENA    => '1'       , -- 1-bit input port-A enable
    REGCEA => '1'       , -- 1-bit input port-A output register enable
    RSTA   => '0'       , -- 1-bit input port-A reset
    WEA    => mem_we( 1 downto 0), -- Input port-A write enable, width defined by Port A depth

    CLKB   => video_clk , -- 1-bit input port-B clock
    DIB    => gnd_8b    , -- Input port-B data, width defined by WRITE_WIDTH_B parameter
    DOB    => tmp8b1    , -- Output port-B data, width defined by READ_WIDTH_B parameter
    ADDRB  => video_in  , -- Input port-B address, width defined by Port B depth
    ENB    => '1'       , -- 1-bit input port-B enable
    REGCEB => '1'       , -- 1-bit input port-B output register enable
    RSTB   => '0'       , -- 1-bit input port-B reset
    WEB    => "0"         -- Input port-B write enable, width defined by Port B depth
  );

inst_ram1 : BRAM_TDP_MACRO
  generic map (
    BRAM_SIZE => "36Kb", -- Target BRAM, "18Kb" or "36Kb"
    DEVICE => "7SERIES", -- Target Device: "VIRTEX5", "VIRTEX6", "7SERIES", "SPARTAN6"
    DOA_REG => 0, -- Optional port A output register (0 or 1)
    DOB_REG => 0, -- Optional port B output register (0 or 1)
    INIT_A => X"000000000", -- Initial values on A output port
    INIT_B => X"000000000", -- Initial values on B output port
    INIT_FILE => "NONE",
    READ_WIDTH_A => 16,  -- Valid values are 1-36 (19-36 only valid when BRAM_SIZE="36Kb")
    READ_WIDTH_B => 8,  -- Valid values are 1-36 (19-36 only valid when BRAM_SIZE="36Kb")
    SIM_COLLISION_CHECK => "ALL", -- Collision check enable "ALL", "WARNING_ONLY", "GENERATE_X_ONLY" or "NONE"
    SRVAL_A => X"000000000",  -- Set/Reset value for A port output
    SRVAL_B => X"000000000",  -- Set/Reset value for B port output
    WRITE_MODE_A => "WRITE_FIRST", -- "WRITE_FIRST", "READ_FIRST" or "NO_CHANGE"
    WRITE_MODE_B => "WRITE_FIRST", -- "WRITE_FIRST", "READ_FIRST" or "NO_CHANGE"
    WRITE_WIDTH_A => 16, -- Valid values are 1-36 (19-36 only valid when BRAM_SIZE="36Kb")
    WRITE_WIDTH_B => 8  -- Valid values are 1-36 (19-36 only valid when BRAM_SIZE="36Kb")
  )
  port map (
    CLKA   => s_axi_aclk, -- 1-bit input port-A clock
    DIA    => dia1, -- Input port-A data, width defined by WRITE_WIDTH_A parameter
    DOA    => doa1, -- Output port-A data, width defined by READ_WIDTH_A parameter
    ADDRA  => mem_addr  , -- Input port-A address, width defined by Port A depth
    ENA    => '1'       , -- 1-bit input port-A enable
    REGCEA => '1'       , -- 1-bit input port-A output register enable
    RSTA   => '0'       , -- 1-bit input port-A reset
    WEA    => mem_we( 3 downto 2), -- Input port-A write enable, width defined by Port A depth

    CLKB   => video_clk , -- 1-bit input port-B clock
    DIB    => gnd_8b    , -- Input port-B data, width defined by WRITE_WIDTH_B parameter
    DOB    => tmp8b2    , -- Output port-B data, width defined by READ_WIDTH_B parameter
    ADDRB  => video_in  , -- Input port-B address, width defined by Port B depth
    ENB    => '1'       , -- 1-bit input port-B enable
    REGCEB => '1'       , -- 1-bit input port-B output register enable
    RSTB   => '0'       , -- 1-bit input port-B reset
    WEB    => "0"         -- Input port-B write enable, width defined by Port B depth
  );

lut_out( 7 downto 0)               <= tmp8b1;
lut_out(lut_out'length-1 downto 8) <= tmp8b2(lut_out'length-9 downto 0);

end generate;

end rtl;
