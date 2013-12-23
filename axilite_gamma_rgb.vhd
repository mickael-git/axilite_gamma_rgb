------------------------------------------------------------------------
--  axilite_gamma_rgb.vhd
--  latency = 2 clk
--
--  Copyright (C) 2013 M.FORET
--
--  This program is free software: you can redistribute it and/or
--  modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation, either version
--  2 of the License, or (at your option) any later version.
------------------------------------------------------------------------

-- Apply 1 LUT on each input component
-- LUT 12b in -> 8b out

-- Addresses of LUTs (byte address)
-- R : 0*4096 = 0
-- G : 1*4096 = 0x1000
-- B : 2*4096 = 0x2000

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.axi3ml_pkg.all;  -- axi-lite records

entity axilite_gamma_rgb is
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
    clk_in        : in  std_logic;
    valid_in      : in  std_logic;
    R_in          : in  std_logic_vector(11 downto 0);  -- unsigned
    G_in          : in  std_logic_vector(11 downto 0);  -- unsigned
    B_in          : in  std_logic_vector(11 downto 0);  -- unsigned

    clk_out       : out std_logic;
    valid_out     : out std_logic;
    R_out         : out std_logic_vector( 7 downto 0);  -- unsigned
    G_out         : out std_logic_vector( 7 downto 0);  -- unsinged
    B_out         : out std_logic_vector( 7 downto 0)   -- unsigned
  );
end entity;

architecture rtl of axilite_gamma_rgb is

constant DATA_WIDTH_IN     : positive := R_in'length;
constant DATA_WIDTH_OUT    : positive := R_out'length;

signal Rg    : std_logic_vector(DATA_WIDTH_OUT-1 downto 0);
signal Gg    : std_logic_vector(DATA_WIDTH_OUT-1 downto 0);
signal Bg    : std_logic_vector(DATA_WIDTH_OUT-1 downto 0);

signal R_out_i  : std_logic_vector(DATA_WIDTH_OUT-1 downto 0);
signal G_out_i  : std_logic_vector(DATA_WIDTH_OUT-1 downto 0);
signal B_out_i  : std_logic_vector(DATA_WIDTH_OUT-1 downto 0);

signal valid_delay : std_logic_vector( 1 downto 0);

begin

gamma0 : entity work.axilite_3_lut
    generic map (
        DATA_WIDTH_OUT  => DATA_WIDTH_OUT
    )
    port map (
        -- ========= AXI
        s_axi_aclk     => s_axi_aclk,
        --
        s_axi_areset_n => s_axi_areset_n,

        -- write interface
        s_axi_wi   => s_axi_wi,
        s_axi_wo   => s_axi_wo,

        -- read interface
        s_axi_ri   => s_axi_ri,
        s_axi_ro   => s_axi_ro,

        -- ========= video interface
        video_clk  => clk_in,
        lut0_in    => R_in,
        lut1_in    => G_in,
        lut2_in    => B_in,
        lut0_out   => Rg,
        lut1_out   => Gg,
        lut2_out   => Bg
    );

-- just put in registers
    process(clk_in)
    begin
        if rising_edge(clk_in) then
            R_out_i <= Rg;
            G_out_i <= Gg;
            B_out_i <= Bg;
        end if;
    end process;

-- delay for valid
    process(clk_in)
    begin
        if rising_edge(clk_in) then
            valid_delay <= valid_delay(valid_delay'high-1 downto 0) & valid_in;
        end if;
    end process;

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clk_out   <= clk_in;
valid_out <= valid_delay(valid_delay'high);
R_out     <= R_out_i;
G_out     <= G_out_i;
B_out     <= B_out_i;

end rtl;
