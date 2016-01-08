-- -------------------------------------------------------------------
-- Receiver component.
-- -------------------------------------------------------------------
library IEEE;
use IEEE.std_logic_1164.ALL;
use IEEE.std_logic_unsigned.ALL;
use IEEE.numeric_std.all;

entity Receiver is
    port( clock : in std_logic;
          rx : in std_logic;
          recv_byte : out std_logic_vector( 7 downto 0 );
          ready : out std_logic );
end Receiver;

architecture recv_arch of Receiver is
begin

recv_process : process ( clock, rx )
type MyState is ( START, DATA, STOP );
variable state : MyState := START;
variable count : integer range 0 to 8 := 0;
begin
    if clock'event AND clock = '0' then
        case state is
            when START =>
                ready <= '0';
                if rx = '0' then
                    count := 0;
                    state := DATA;
                end if;
            when DATA =>
                case count is
                    when 0 =>
                        recv_byte <= "0000000" & rx;
                        count := 1;
                    when 1 =>
                        recv_byte( 1 ) <= rx;
                        count := 2;
                    when 2 =>
                        recv_byte( 2 ) <= rx;
                        count := 3;
                    when 3 =>
                        recv_byte( 3 ) <= rx;
                        count := 4;
                    when 4 =>
                        recv_byte( 4 ) <= rx;
                        count := 5;
                    when 5 =>
                        recv_byte( 5 ) <= rx;
                        count := 6;
                    when 6 =>
                        recv_byte( 6 ) <= rx;
                        count := 7;
                    when 7 =>
                        recv_byte( 7 ) <= rx;
                        state := STOP;
                    when others =>
                        state := START;
                end case;
            when STOP =>
                ready <= rx;
                state := START;
         end case;
    end if;
end process;
end recv_arch;

-- -------------------------------------------------------------------
-- Transmitter component.
-- -------------------------------------------------------------------
library IEEE;
use IEEE.std_logic_1164.ALL;
use IEEE.std_logic_unsigned.ALL;
use IEEE.numeric_std.all;

entity Transmitter is
    port( clock : in std_logic;
          load : in std_logic;
          send_byte : in std_logic_vector( 7 downto 0 );
          busy : out std_logic;
          tx : out std_logic );
end Transmitter;

architecture send_arch of Transmitter is
    signal sending_byte : std_logic_vector( 7 downto 0 );
begin

send_process : process ( clock, load )
type MyState is ( START, DATA, STOP );
variable state : MyState := START;
variable count : integer range 0 to 8 := 0;
begin
    if clock'event AND clock = '1' then
        case state is
            when START =>
                if load = '1' then
                    sending_byte <= send_byte;
                    tx <= '0';
                    busy <= '1';
                    count := 0;
                    state := DATA;
                else
                    busy <= '0';
                    tx <= '1';
                end if;
            when DATA =>
                --busy<='1';
                case count is
                    when 0 =>
                        tx <= sending_byte( 0 );
                    when 1 =>
                        tx <= sending_byte( 1 );
                    when 2 =>
                        tx <= sending_byte( 2 );
                    when 3 =>
                        tx <= sending_byte( 3 );
                    when 4 =>
                        tx <= sending_byte( 4 );
                    when 5 =>
                        tx <= sending_byte( 5 );
                    when 6 =>
                        tx <= sending_byte( 6 );
                    when 7 =>
                        tx <= sending_byte( 7 );
                        state := STOP;
                    when others =>
                        state := STOP;
                end case;
                count := count + 1;
            when STOP =>
                busy <= '0';
                tx <= '1';
                state := START;
            when others =>
                busy<='0';
                state := START;
         end case;
    end if;
end process;
end send_arch;

-- -------------------------------------------------------------------
-- Clock component.
-- -------------------------------------------------------------------
library IEEE;
use IEEE.std_logic_1164.ALL;
use IEEE.std_logic_unsigned.ALL;

entity Clock is
    port( clk : in std_logic; reset : in std_logic; clock : out std_logic );
end Clock;

architecture clock_arch of Clock is
begin
clock_process : process ( clk, reset )
constant FREQ : integer := 57600; -- Hz
constant CLOCK_FREQ : integer := 50000000; -- Hz
constant TOTAL_TICKS_COUNT : integer := CLOCK_FREQ / FREQ;
constant TICKS_COUNT : integer := TOTAL_TICKS_COUNT / 2;

variable count:integer range 0 to TOTAL_TICKS_COUNT;

begin
    if reset = '0' then
        count := 0;
    elsif clk'event AND clk = '1' then
        if count < TICKS_COUNT then
            clock <= '1';
            count:=count + 1;
        elsif count < TOTAL_TICKS_COUNT then
            clock <= '0';
            count := count + 1;
        else
            count := 0;
        end if;
     end if;
end process;
end clock_arch;

-- -------------------------------------------------------------------
-- Uart component
-- -------------------------------------------------------------------
library IEEE;
use IEEE.std_logic_1164.ALL;

entity Uart is
    port( clk : in std_logic; 
          rx : in std_logic; 
          tx : out std_logic;
          signal rx_ready : out std_logic;
          signal recv_byte : out std_logic_vector( 7 downto 0 );
          signal tx_busy : out std_logic;
          signal tx_load : in std_logic;
          signal send_byte : in std_logic_vector( 7 downto 0 ) );
end Uart;

architecture arch of Uart is
    component Clock is 
        port( clk : in std_logic; reset : in std_logic; clock : out std_logic );
    end component;
    
    component Receiver is
        port( clock : in std_logic;
              rx : in std_logic;
              recv_byte : out std_logic_vector( 7 downto 0 );
              ready : out std_logic );
    end component;

    Component Transmitter is
    port( clock : in std_logic;
          load : in std_logic;
          send_byte : in std_logic_vector( 7 downto 0 );
          busy : out std_logic;
          tx : out std_logic );
    end component;

    signal clock_tmp_rx : std_logic;
    signal reset_clock_rx : std_logic;

    --signal clock_tmp_tx : std_logic;
    --signal reset_clock_tx : std_logic;
begin

clock_sync_process : process ( clk, rx )
variable old_rx : std_logic := '1';
begin
    if clk'event AND clk = '1' then
        reset_clock_rx <= not ( rx xor old_rx );
        old_rx := rx;
    end if;
end process;

    -- reset_clock_tx <= '1';

    clock_rx : Clock port map( clk, reset_clock_rx, clock_tmp_rx );
    -- clock_tx : Clock port map( clk, reset_clock_tx, clock_tmp_tx );

    receiver_comp : Receiver port map( clock_tmp_rx, rx, recv_byte, rx_ready );
    transmitter_comp : Transmitter port map( clock_tmp_rx, tx_load, send_byte, tx_busy, tx );
end arch;

-- -------------------------------------------------------------------
-- Echo component
-- -------------------------------------------------------------------
library IEEE;
use IEEE.std_logic_1164.ALL;

entity UartEcho is
    port( clk : in std_logic; rx : in std_logic; tx : out std_logic; led : out std_logic );
end UartEcho;

architecture arch of UartEcho is

    component Uart is
        port( clk : in std_logic; 
              rx : in std_logic; 
              tx : out std_logic;
              signal rx_ready : out std_logic;
              signal recv_byte : out std_logic_vector( 7 downto 0 );
              signal tx_busy : out std_logic;
              signal tx_load : in std_logic;
              signal send_byte : in std_logic_vector( 7 downto 0 ) );
    end component;

    signal rx_ready : std_logic;
    signal recv_byte : std_logic_vector( 7 downto 0 );
    signal tx_busy : std_logic;
    signal tx_load : std_logic;
    signal send_byte : std_logic_vector( 7 downto 0 );

begin

echo_process : process( clk, rx_ready, tx_busy, recv_byte )
type MyState is ( IDLE, RECV, SEND );
variable state : MyState := IDLE;
variable old_ready : std_logic := '0';
begin
    if clk'event AND clk = '1' then
        case state is
            when IDLE =>
                tx_load <= '0';
                if rx_ready = '1' and old_ready = '0' then
                     send_byte <= recv_byte;
                    state := RECV;
                end if;
            when RECV =>
                if tx_busy = '0' then
                    tx_load <= '1';
                    state := SEND;
                end if;
            when SEND =>
                if tx_busy = '1' then
                    tx_load <= '0';
                    state := IDLE;
                end if;
            when others =>
                state := IDLE;
        end case;

        old_ready := rx_ready;

    end if;
end process;

    led <= not rx;

    uart_comp : Uart port map( clk, rx, tx, rx_ready, recv_byte, tx_busy, tx_load, send_byte );

end arch;