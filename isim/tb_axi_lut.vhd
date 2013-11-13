------------------------------------------------------------------------
--  tb_axi_lut.vhd
--  testbench
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
use ieee.numeric_std.all;

use work.axi3m_pkg.all;  -- axi records

use work.pkg_axi_lut.all;
use work.pkg_axi_master_model.all;
use work.pkg_tools_tb.all;

entity testbench is
end;

architecture tb of testbench is

constant DATA_WIDTH_IN  : positive := 12;  -- cannot be changed
constant DATA_WIDTH_OUT : positive := 10;

signal s_axi_wo : axi3m_write_in_r;
signal s_axi_wi : axi3m_write_out_r;

signal s_axi_ro : axi3m_read_in_r;
signal s_axi_ri : axi3m_read_out_r;

signal axi_clk          : std_logic := '0';
signal axi_resetn       : std_logic := '1';

signal video_clk        : std_logic := '0';
signal video_in         : std_logic_vector(DATA_WIDTH_IN-1  downto 0);
signal lut_out          : std_logic_vector(DATA_WIDTH_OUT-1 downto 0);

  procedure waiting(signal clk : std_logic; nb : integer) is
  begin
    for i in 1 to nb loop
      wait until rising_edge(clk);
    end loop;
  end;

begin

axi_clk <= not(axi_clk) after 10 ns;

video_clk <= not(video_clk) after 5 ns;

-- AXI tests
process
  variable data   : std_logic_vector(31 downto 0);
  variable addr   : std_logic_vector(31 downto 0);
begin
  axi_resetn <= '0';
  waiting(axi_clk, 5);
  axi_resetn <= '1';

  display(string'("Write values ..."));
  for i in 0 to 100 loop
    addr := std_logic_vector(to_unsigned(i*4, addr'length));
    data := std_logic_vector(to_unsigned(i, data'length));
    single_write(axi_clk, s_axi_wo, s_axi_wi, data, addr);
  end loop;

  display(string'("Read values ..."));
  for i in 0 to 100 loop
    addr := std_logic_vector(to_unsigned(i*4, addr'length));
    single_read(axi_clk, s_axi_ro, s_axi_ri, data, addr);
    if (unsigned(data) /= i) then
      display("Error for address :" & integer'image(i));
    end if;
  end loop;

  display(string'("Burst Write ..."));
  addr := std_logic_vector(to_unsigned(10*4, addr'length));
  burst_write(axi_clk, s_axi_wo, s_axi_wi, "./data.txt", addr);

  display(string'("Burst Read ..."));
  addr := std_logic_vector(to_unsigned(10*4, addr'length));
  burst_read(axi_clk, s_axi_ro, s_axi_ri, "./check.txt", addr);

  display("End of simulation");
  wait  for 50 ns;

  report "End of test (this is not a failure)"
    severity failure;
  wait;
end process;

-- data through LUT
process
  variable count  : integer;
begin

  count := 0;
  wait for 10 us;

  wait until rising_edge(video_clk);

  for i in 0 to 100 loop
    video_in <= std_logic_vector(to_unsigned(count, video_in'length));
    wait until rising_edge(video_clk);
    count := count + 1;
  end loop;

  wait;
end process;


-- /////////////////////////////////////////////////////////////////////

uut0 : axi_lut
  generic map (
    DATA_WIDTH_OUT   => DATA_WIDTH_OUT
  )
  port map (
    s_axi_aclk     => axi_clk    ,

    s_axi_areset_n => axi_resetn ,

    s_axi_wi       => s_axi_wi   ,
    s_axi_wo       => s_axi_wo   ,

    s_axi_ri       => s_axi_ri   ,
    s_axi_ro       => s_axi_ro   ,

    video_clk      => video_clk  ,
    video_in       => video_in   ,
    lut_out        => lut_out
  );

end tb;
