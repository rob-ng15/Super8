// SL 2020-09-05
// Silice HDMI driver
//
// 640x480, 250MHz TMDS from 25MHz pixel clock
//
// Currently limited to the ULX3S, but should be relatively easy to port,
// pending pll and differential serial output primitives
//
// See also
// - https://www.digikey.com/eewiki/pages/viewpage.action?pageId=36569119
// - https://www.fpga4fun.com/HDMI.html
// - https://github.com/lawrie/ulx3s_examples/blob/master/hdmi/tmds_encoder.v
//
//      GNU AFFERO GENERAL PUBLIC LICENSE
//        Version 3, 19 November 2007
//
//  A copy of the license full text is included in
//  the distribution, please refer to it for details.

import('hdmi_clock.v')
import('ddr.v')
import('hdmi_ddr_crgb.v')

// ----------------------------------------------------

algorithm tmds_encoder(
  input   uint8  data,
  input   uint2  ctrl,
  input   uint1  data_or_ctrl,
  output  uint10 tmds
) <autorun> {

  uint9 q_m             = uninitialized;
  int5  dc_bias         = uninitialized;

  // tracks 'number on ones' in input
  uint4 num_ones        := data[0,1] + data[1,1] + data[2,1] + data[3,1]
                         + data[4,1] + data[5,1] + data[6,1] + data[7,1];
  // tracks 'numbers of ones minus number of zeros' in internal byte
  int5  diff_ones_zeros := q_m[0,1] + q_m[1,1] + q_m[2,1] + q_m[3,1]
                         + q_m[4,1] + q_m[5,1] + q_m[6,1] + q_m[7,1] - 6d4;

  // XOR chain on input
  int1  xored1          := data[1,1] ^ data[0,1];
  int1  xored2          := data[2,1] ^ xored1;
  int1  xored3          := data[3,1] ^ xored2;
  int1  xored4          := data[4,1] ^ xored3;
  int1  xored5          := data[5,1] ^ xored4;
  int1  xored6          := data[6,1] ^ xored5;
  int1  xored7          := data[7,1] ^ xored6;

  // XNOR chain on input
  int1  xnored1         := ~(data[1,1] ^ data[0,1]);
  int1  xnored2         := ~(data[2,1] ^ xnored1);
  int1  xnored3         := ~(data[3,1] ^ xnored2);
  int1  xnored4         := ~(data[4,1] ^ xnored3);
  int1  xnored5         := ~(data[5,1] ^ xnored4);
  int1  xnored6         := ~(data[6,1] ^ xnored5);
  int1  xnored7         := ~(data[7,1] ^ xnored6);

  always {
    // choice of encoding scheme (xor / xnor)
    if ((num_ones > 4) || (num_ones == 4 && data[0,1] == 0)) {
      q_m = { 1b0 , {xnored7,xnored6,xnored5,xnored4,xnored3,xnored2,xnored1} , data[0,1] };
    } else {
      q_m = { 1b1 , {xored7,xored6,xored5,xored4,xored3,xored2,xored1} , data[0,1] };
    }
    if (data_or_ctrl) {
      // output data
      if (dc_bias == 0 || diff_ones_zeros == 0) {
        tmds      = {~q_m[8,1] , q_m[8,1], (q_m[8,1] ? q_m[0,8] : ~q_m[0,8])};
        if (q_m[8,1] == 0) {
          dc_bias = dc_bias - diff_ones_zeros;
        } else {
          dc_bias = dc_bias + diff_ones_zeros;
        }
      } else {
        if (  (dc_bias > 0 && diff_ones_zeros > 0)
           || (dc_bias < 0 && diff_ones_zeros < 0) ) {
          tmds    = {1b1, q_m[8,1], ~q_m[0,8] };
          dc_bias = dc_bias + q_m[8,1] - diff_ones_zeros;
        } else {
          tmds    = {1b0,q_m};
          dc_bias = dc_bias - (~q_m[8,1]) + diff_ones_zeros;
        }
      }
    } else {
      // output control
      switch (ctrl) {
        case 2b00: { tmds = 10b1101010100; }
        case 2b01: { tmds = 10b0010101011; }
        case 2b10: { tmds = 10b0101010100; }
        case 2b11: { tmds = 10b1010101011; }
      }
      dc_bias = 0;
    }
  }

}

// ----------------------------------------------------

algorithm hdmi_ddr_shifter(
  input   uint10 data_r,
  input   uint10 data_g,
  input   uint10 data_b,
  output  uint8  p_outbits,
) <autorun> {
  uint5  mod5    = 1; // circular mod5 buffer, same trick as the oled 'osc' in ice-v!
  uint10 shift_r = uninitialized;
  uint10 shift_g = uninitialized;
  uint10 shift_b = uninitialized;
  uint2  clkbits = uninitialized;
  always {
    shift_r   = (mod5[0,1] == 1) ? data_r : shift_r[2,8];
    shift_g   = (mod5[0,1] == 1) ? data_g : shift_g[2,8];
    shift_b   = (mod5[0,1] == 1) ? data_b : shift_b[2,8];
    clkbits   = mod5[2,1] ? 2b01 : {2{mod5[0,1]|mod5[1,1]}};
    p_outbits = { clkbits , shift_b[0,2] , shift_g[0,2] , shift_r[0,2] };
    mod5      = {mod5[0,4],mod5[4,1]};
  }
}

// ----------------------------------------------------

// Expects to run at 25 MHz (hdmi pixel clock)
algorithm hdmi(
  output  uint10 x,
  output  uint10 y,
  output  uint1  active,
  output  uint1  vblank,
  output! uint4  gpdi_dp,
  input   uint8  red,
  input   uint8  green,
  input   uint8  blue
)  {

  uint10 cntx  = 0;
  uint9  cnty  = 0;

  uint1 hsync      := (cntx > 655) && (cntx < 752);
  uint1 vsync      := (cnty > 489) && (cnty < 492);

  uint2 sync_ctrl   = uninitialized;
  uint2 null_ctrl  := 0;

  // pll for tmds
  uint1  half_hdmi_clk = uninitialized;
  hdmi_clock pll(
    clk           <: clock,         //  25 MHz
    half_hdmi_clk :> half_hdmi_clk, // 125 MHz (half 250MHz HDMI, double data rate output)
  );

  uint10 tmds_red       = uninitialized;
  uint10 tmds_green     = uninitialized;
  uint10 tmds_blue      = uninitialized;

  uint8  latch_red      = uninitialized;
  uint8  latch_green    = uninitialized;
  uint8  latch_blue     = uninitialized;
  uint2  prev_sync_ctrl = uninitialized;
  uint1  prev_active    = uninitialized;

  // encoders
  // => we use <:: to bind values from cycle start (ignoring changes during current cycle)
  tmds_encoder tmdsR(
    data        <:: latch_red,
    ctrl        <:  null_ctrl,
    data_or_ctrl<:: prev_active,
    tmds         :> tmds_red
  );
  tmds_encoder tmdsG(
    data        <:: latch_green,
    ctrl        <:  null_ctrl,
    data_or_ctrl<:: prev_active,
    tmds         :> tmds_green
  );
  tmds_encoder tmdsB(
    data        <:: latch_blue,
    ctrl        <:: prev_sync_ctrl,
    data_or_ctrl<:: prev_active,
    tmds         :> tmds_blue
  );

  // shifter
  uint8 crgb_pos = 0;
  hdmi_ddr_shifter shift<@half_hdmi_clk>(
    data_r  <: tmds_red,
    data_g  <: tmds_green,
    data_b  <: tmds_blue,
    p_outbits :> crgb_pos
  );

  hdmi_ddr_crgb hdmi_out(
    clock      <: half_hdmi_clk,
    crgb_twice <: crgb_pos,
    out_pin    :> gpdi_dp
  );

  always {

    // record previous state of sync_ctrl and active,
    // we receive the r,b,g value for the x,y set below with a one cycle latency
    // these are then latched for the following cycle
    // thus we have to delay corresponding sync and active two cycles
    prev_sync_ctrl = sync_ctrl;
    prev_active    = active;
    // synchronization bits
    sync_ctrl      = {vsync,hsync};
    // output active area
    active         = (cntx < 640) && (cnty < 480);
    // output vblank
    vblank         = (cnty >= 480);
    // output x,y
    x              = cntx;
    y              = cnty;
    // => we will get color result on next cycle

    // update coordinates
    cnty        = (cntx == 799) ? (cnty == 524 ? 0 : (cnty + 1)) : cnty;
    cntx        = (cntx == 799) ? 0 : (cntx + 1);

    // latch r,b,g received at this cycle, for previous coord
    // will be fed into HDMI encoders next cycle
    latch_red   = red;
    latch_green = green;
    latch_blue  = blue;

  }
}

// ----------------------------------------------------
