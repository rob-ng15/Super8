// DEFINE I/O CLOCKS
$$ uart_in_clock_freq_mhz = 50

// ADDRESS WIDTH OF THE SDRAM ( 26 bits is 32Mb )
$$ sdram_addr_width = 26

// CLOCKS
$$if not SIMULATION then
import('../common/clock_SYSTEM8.v')
$$end

// HDMI for FPGA, VGA for SIMULATION
$$if HDMI then
$include('../common/hdmi.ice')
$$end
$$if VGA then
$include('vga.si')
$$end

$include('../common/uart.si')
$include('../common/ps2.si')

// CLEAN RESET
$$ clean_reset_width = 1
$include('../common/clean_reset.si')

// SYSTEM8
$include('../Super8.si')
$include('../6502.si')
$include('../MAIN_CPU.si')
$include('../IO_CPU.si')
$include('../TERMINAL_CPU.si')
$include('../VIDEO_DISPLAY_MULTIPLEX.si')
