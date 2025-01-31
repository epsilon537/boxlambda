#ifndef __SIM_CORE_H_
#define __SIM_CORE_H_
#include "pads.h"

struct pad_s sim_trace[] = {
    { (char*)"sim_trace", 1, NULL },
    { NULL, 0, NULL }
};

struct pad_s clk[] = {
    { (char*)"clk", 1, NULL },
    { NULL, 0, NULL }
};

struct pad_s init_done[] = {
    { (char*)"init_done", 1, NULL },
    { NULL, 0, NULL }
};

struct pad_s init_error[] = {
    { (char*)"init_error", 1, NULL },
    { NULL, 0, NULL }
};

struct pad_s wb_ctrl[] = {
    { (char*)"adr", 30, NULL },
    { (char*)"dat_w", 32, NULL },
    { (char*)"dat_r", 32, NULL },
    { (char*)"sel", 4, NULL },
    { (char*)"cyc", 1, NULL },
    { (char*)"stb", 1, NULL },
    { (char*)"ack", 1, NULL },
    { (char*)"we", 1, NULL },
    { (char*)"cti", 3, NULL },
    { (char*)"bte", 2, NULL },
    { (char*)"err", 1, NULL },
    { NULL, 0, NULL }
};

struct pad_s user_clk[] = {
    { (char*)"user_clk", 1, NULL },
    { NULL, 0, NULL }
};

struct pad_s user_clkx2[] = {
    { (char*)"user_clkx2", 1, NULL },
    { NULL, 0, NULL }
};

struct pad_s user_rst[] = {
    { (char*)"user_rst", 1, NULL },
    { NULL, 0, NULL }
};

struct pad_s user_port_wishbone_0[] = {
    { (char*)"adr", 26, NULL },
    { (char*)"dat_w", 32, NULL },
    { (char*)"dat_r", 32, NULL },
    { (char*)"sel", 4, NULL },
    { (char*)"cyc", 1, NULL },
    { (char*)"stb", 1, NULL },
    { (char*)"ack", 1, NULL },
    { (char*)"we", 1, NULL },
    { (char*)"err", 1, NULL },
    { NULL, 0, NULL }
};

#ifndef __cplusplus
void litex_sim_init(void **out);
#endif

#endif /* __SIM_CORE_H_ */
