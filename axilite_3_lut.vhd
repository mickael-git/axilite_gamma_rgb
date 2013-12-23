------------------------------------------------------------------------
--  axilite_3_lut.vhd
--  3 lut
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

use work.axi3ml_pkg.all;  -- axi-lite records

entity axilite_3_lut is
    generic (
        DATA_WIDTH_OUT   : positive := 8  -- data width output (1 to 16)
    );
    port (
        -- ========= AXI
        s_axi_aclk     : in  std_logic;
        --
        s_axi_areset_n : in  std_logic;
        
        -- write interface
        s_axi_wi : in  axi3ml_write_out_r;
        s_axi_wo : out axi3ml_write_in_r;
        
        -- read interface
        s_axi_ri : in  axi3ml_read_out_r;
        s_axi_ro : out axi3ml_read_in_r;
        
        -- ========= video interface
        video_clk     : in  std_logic;
        lut0_in       : in  std_logic_vector(11 downto 0);
        lut0_out      : out std_logic_vector(DATA_WIDTH_OUT-1 downto 0);
        lut1_in       : in  std_logic_vector(11 downto 0);
        lut1_out      : out std_logic_vector(DATA_WIDTH_OUT-1 downto 0);
        lut2_in       : in  std_logic_vector(11 downto 0);
        lut2_out      : out std_logic_vector(DATA_WIDTH_OUT-1 downto 0)
    );
end entity;

architecture rtl of axilite_3_lut is

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

constant DATA_WIDTH_IN   : natural := lut0_in'length;
-- compute address width of memory = DATA_WIDTH_IN + log2(DATA_WIDTH_OUT) - ln2(32)
constant ADDR_WIDTH      : natural := DATA_WIDTH_IN + log2(DATA_WIDTH_OUT) - 5 + 2;

signal mem_addr      : std_logic_vector(ADDR_WIDTH-1 downto 0);
signal mem_we        : std_logic_vector( 3 downto 0);
signal mem_din       : std_logic_vector(31 downto 0);
signal mem_dout      : std_logic_vector(31 downto 0);

signal addr          : std_logic_vector(ADDR_WIDTH-1-2 downto 0);

signal mem_dout0     : std_logic_vector(mem_dout'range);
signal mem_dout1     : std_logic_vector(mem_dout'range);
signal mem_dout2     : std_logic_vector(mem_dout'range);

signal mem_wea0      : std_logic_vector( 3 downto 0);
signal mem_wea1      : std_logic_vector( 3 downto 0);
signal mem_wea2      : std_logic_vector( 3 downto 0);


alias sel_mem        : std_logic_vector( 1 downto 0) is mem_addr(ADDR_WIDTH-1 downto ADDR_WIDTH-1-1);

begin


axi_to_mem0 : entity work.axilite_to_memory
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

  process(sel_mem, mem_dout0, mem_dout1, mem_dout2)
  begin
    case sel_mem is
      when "01"    => mem_dout <= mem_dout1;
      when "10"    => mem_dout <= mem_dout2;
      when others  => mem_dout <= mem_dout0;
    end case;
  end process;

mem_wea0 <= mem_we when sel_mem="00" else (others=>'0');
mem_wea1 <= mem_we when sel_mem="01" else (others=>'0');
mem_wea2 <= mem_we when sel_mem="10" else (others=>'0');

addr <= mem_addr(ADDR_WIDTH-1-2 downto 0);

lut0: entity work.ram_asym
  generic map (
    ADDR_WIDTHA    => ADDR_WIDTH-2   ,
    ADDR_WIDTHB    => DATA_WIDTH_IN  ,
    DATA_WIDTHB    => DATA_WIDTH_OUT
  )
  port map (
    clka           => s_axi_aclk ,
    mem_addra      => addr       ,
    mem_wea        => mem_wea0   ,
    mem_dina       => mem_din    ,
    mem_douta      => mem_dout0  ,
    --
    clkb           => video_clk  ,
    mem_addrb      => lut0_in    ,
    mem_doutb      => lut0_out
  );

lut1: entity work.ram_asym
  generic map (
    ADDR_WIDTHA    => ADDR_WIDTH-2   ,
    ADDR_WIDTHB    => DATA_WIDTH_IN  ,
    DATA_WIDTHB    => DATA_WIDTH_OUT
  )
  port map (
    clka           => s_axi_aclk ,
    mem_addra      => addr       ,
    mem_wea        => mem_wea1   ,
    mem_dina       => mem_din    ,
    mem_douta      => mem_dout1  ,
    --
    clkb           => video_clk  ,
    mem_addrb      => lut1_in    ,
    mem_doutb      => lut1_out
  );

lut2: entity work.ram_asym
  generic map (
    ADDR_WIDTHA    => ADDR_WIDTH-2   ,
    ADDR_WIDTHB    => DATA_WIDTH_IN  ,
    DATA_WIDTHB    => DATA_WIDTH_OUT
  )
  port map (
    clka           => s_axi_aclk ,
    mem_addra      => addr       ,
    mem_wea        => mem_wea2   ,
    mem_dina       => mem_din    ,
    mem_douta      => mem_dout2  ,
    --
    clkb           => video_clk  ,
    mem_addrb      => lut2_in    ,
    mem_doutb      => lut2_out
  );


end rtl;
