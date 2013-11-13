------------------------------------------------------------------------
--  pkg_axi_lut.vhd
--  package with all components
--
--  Copyright (C) 2013 M.FORET
--
--  This program is free software: you can redistribute it and/or
--  modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation, either version
--  2 of the License, or (at your option) any later version.
------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.ALL;

use work.axi3m_pkg.all;  -- axi records

package pkg_axi_lut is

  component axi_to_memory
    generic (
      ADDR_WIDTH            : integer := 15  -- address width (32 bits word)
    );
    port (
      -- ========= AXI
      s_axi_aclk     : in  std_logic;
      --
      s_axi_areset_n : in  std_logic;

      -- wite interface
      s_axi_wi : in  axi3m_write_out_r;
      s_axi_wo : out axi3m_write_in_r;

      -- read interface
      s_axi_ri : in  axi3m_read_out_r;
      s_axi_ro : out axi3m_read_in_r;

      -- ========= block ram interface
      mem_addr      : out std_logic_vector(ADDR_WIDTH-1 downto 0);
      mem_we        : out std_logic_vector( 3 downto 0);
      mem_din       : out std_logic_vector(31 downto 0);
      mem_dout      : in  std_logic_vector(31 downto 0)
    );
  end component;

  component axi_lut
    generic (
      DATA_WIDTH_OUT   : positive := 8  -- data width output (1 to 16)
    );
    port (
      -- ========= AXI
      s_axi_aclk     : in  std_logic;
      --
      s_axi_areset_n : in  std_logic;

      -- wite interface
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
  end component;

end pkg_axi_lut;
