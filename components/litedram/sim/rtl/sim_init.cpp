#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "Vsim.h"
#include <verilated.h>
#include "sim_header.h"

extern "C" void litex_sim_init_tracer(void *vsim, long start, long end);
extern "C" void litex_sim_tracer_dump();

extern "C" void litex_sim_dump()
{
}

extern "C" void litex_sim_init(void **out)
{
    Vsim *sim;

    sim = new Vsim;

    litex_sim_init_tracer(sim, 0, -1);

    sim_trace[0].signal = &sim->sim_trace;
    litex_sim_register_pads(sim_trace, (char*)"sim_trace", 0);

    clk[0].signal = &sim->clk;
    litex_sim_register_pads(clk, (char*)"clk", 0);

    init_done[0].signal = &sim->init_done;
    litex_sim_register_pads(init_done, (char*)"init_done", 0);

    init_error[0].signal = &sim->init_error;
    litex_sim_register_pads(init_error, (char*)"init_error", 0);

    wb_ctrl[0].signal = &sim->wb_ctrl_adr;
    wb_ctrl[1].signal = &sim->wb_ctrl_dat_w;
    wb_ctrl[2].signal = &sim->wb_ctrl_dat_r;
    wb_ctrl[3].signal = &sim->wb_ctrl_sel;
    wb_ctrl[4].signal = &sim->wb_ctrl_cyc;
    wb_ctrl[5].signal = &sim->wb_ctrl_stb;
    wb_ctrl[6].signal = &sim->wb_ctrl_ack;
    wb_ctrl[7].signal = &sim->wb_ctrl_we;
    wb_ctrl[8].signal = &sim->wb_ctrl_cti;
    wb_ctrl[9].signal = &sim->wb_ctrl_bte;
    wb_ctrl[10].signal = &sim->wb_ctrl_err;
    litex_sim_register_pads(wb_ctrl, (char*)"wb_ctrl", 0);

    user_clk[0].signal = &sim->user_clk;
    litex_sim_register_pads(user_clk, (char*)"user_clk", 0);

    user_rst[0].signal = &sim->user_rst;
    litex_sim_register_pads(user_rst, (char*)"user_rst", 0);

    user_port_wishbone_0[0].signal = &sim->user_port_wishbone_0_adr;
    user_port_wishbone_0[1].signal = &sim->user_port_wishbone_0_dat_w;
    user_port_wishbone_0[2].signal = &sim->user_port_wishbone_0_dat_r;
    user_port_wishbone_0[3].signal = &sim->user_port_wishbone_0_sel;
    user_port_wishbone_0[4].signal = &sim->user_port_wishbone_0_cyc;
    user_port_wishbone_0[5].signal = &sim->user_port_wishbone_0_stb;
    user_port_wishbone_0[6].signal = &sim->user_port_wishbone_0_ack;
    user_port_wishbone_0[7].signal = &sim->user_port_wishbone_0_we;
    user_port_wishbone_0[8].signal = &sim->user_port_wishbone_0_err;
    litex_sim_register_pads(user_port_wishbone_0, (char*)"user_port_wishbone_0", 0);

    user_port_wishbone_1[0].signal = &sim->user_port_wishbone_1_adr;
    user_port_wishbone_1[1].signal = &sim->user_port_wishbone_1_dat_w;
    user_port_wishbone_1[2].signal = &sim->user_port_wishbone_1_dat_r;
    user_port_wishbone_1[3].signal = &sim->user_port_wishbone_1_sel;
    user_port_wishbone_1[4].signal = &sim->user_port_wishbone_1_cyc;
    user_port_wishbone_1[5].signal = &sim->user_port_wishbone_1_stb;
    user_port_wishbone_1[6].signal = &sim->user_port_wishbone_1_ack;
    user_port_wishbone_1[7].signal = &sim->user_port_wishbone_1_we;
    user_port_wishbone_1[8].signal = &sim->user_port_wishbone_1_err;
    litex_sim_register_pads(user_port_wishbone_1, (char*)"user_port_wishbone_1", 0);

    *out=sim;
}
