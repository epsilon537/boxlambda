#include <curses.h>
#include <getopt.h>
#include <fcntl.h>

// For std::unique_ptr
#include <memory>

#include <string>

#include <SDL2/SDL.h>

// Include common routines
#include <verilated.h>

// Include model header, generated from Verilating "top.v"
#include "Vmodel.h"

#include "verilated_fst_c.h"

//To get access to the verilated model internals.
#include "Vmodel___024root.h"

#include "vera.h"

#define VRAM_SIZE_BYTES (128*1024)
#define VRAM_MAP_BASE 0x10000

//SDL objects:
#define SDL_WINDOW_WIDTH 800

SDL_Event sdl_event;
SDL_Renderer *sdl_renderer;
SDL_Window *sdl_window;
SDL_Texture *sdl_display;

int sdl_x=0/*750*/, sdl_y=0/*523*/;

bool tracing_enable = false;

// Used for tracing.
VerilatedFstC* tfp = new VerilatedFstC;

// Construct a VerilatedContext to hold simulation time, etc.
// Multiple modules (made later below with Vtop) may share the same
// context to share time, or modules may have different contexts if
// they should be independent from each other.
std::unique_ptr<VerilatedContext> contextp{new VerilatedContext}; 

// Construct the Verilated model, from Vmodel.h generated from Verilating this project.
// Using unique_ptr is similar to "Vmodel* top = new Vmodel" then deleting at end.
std::unique_ptr<Vmodel> top{new Vmodel{contextp.get()}};

// Legacy function required only so linking works on Cygwin and MSVC++
double sc_time_stamp() { return 0; }

//Advance simulation by one clock cycle
static void tick() {
  top->clk25 = 1;
  contextp->timeInc(1);
  top->eval();
  if (tracing_enable)
    tfp->dump(contextp->time());
  top->clk25 = 0;
  contextp->timeInc(1);
  top->eval();
  if (tracing_enable)
    tfp->dump(contextp->time());
}

//A very crude external bus write implementation.
void ext_bus_wr(unsigned addr, unsigned data) {
  top->extbus_a = addr;      /* Address */
  top->extbus_d = data;      /* Data (bi-directional) */

  top->extbus_cs_n = 0;   /* Chip select */
  top->extbus_wr_n = 0;   /* Write strobe */
  
  tick();
  tick();
  tick();

  top->extbus_cs_n = !0;   /* Chip select */
  top->extbus_wr_n = !0;   /* Write strobe */
  tick();
  tick();
  tick();
  tick();
}

//This function writes the given data byte to the given address in VERA's VRAM.
void vram_wr(unsigned addr, unsigned char data) {
  ext_bus_wr(VERA_ADDR_L, addr&0xff);
  ext_bus_wr(VERA_ADDR_M, (addr>>8)&0xff);
  ext_bus_wr(VERA_ADDR_H, (addr>>16)&1);
  ext_bus_wr(VERA_DATA0, data);
}

//Returns <0 if unsuccessful
int load_bin_file_into_vram(const char* vram_bin_filename) {
  static unsigned char buffer[VRAM_SIZE_BYTES];
  int n;

  printf("Loading into VRAM: %s\n\r", vram_bin_filename);

  FILE *f = fopen(vram_bin_filename, "rb");
  if (f) {
    n = fread(buffer, 1, VRAM_SIZE_BYTES, f);

    for (int ii=0; ii<n; ii++) {
      vram_wr(ii, buffer[ii]);
    }
  }   
  else
  {
    printf("File not found: %s\n\r", vram_bin_filename);
    return -1;
  }

  printf("Done\n\r");
  fclose(f);
  return 0;
}

int main(int argc, char** argv, char** env) {
    // Prevent unused variable warnings
    if (false && argc && argv && env) {}

    // Set debug level, 0 is off, 9 is highest presently used
    // May be overridden by commandArgs argument parsing
    contextp->debug(0);

    // Randomization reset policy
    // May be overridden by commandArgs argument parsing
    contextp->randReset(2);

    // Verilator must compute traced signals
    contextp->traceEverOn(true);
    
    // Pass arguments so Verilated code can see them, e.g. $value$plusargs
    // This needs to be called before you create any model
    contextp->commandArgs(argc, argv);

    bool attach_debugger = false;
    bool interactive_mode = false;
    char *vram_bin_filename = NULL;

    // Command line processing
    for(;;) {
      switch(getopt(argc, argv, "thf:")) {
      case 't':
        printf("Tracing enabled\n");
        tracing_enable = true;
        continue;

      case 'f':
        vram_bin_filename = optarg;
        continue;

      case '?':
      case 'h':
      default :
        printf("\nVmodel Usage:\n");
        printf("-h: print this help\n");
        printf("-t: enable tracing.\n");
        printf("-f <vram.bin>: load given bin file into vram.\n");
        return 0;
        break;
	    
      case -1:
        break;
      }

      break;
    }

    //SDL setup
    SDL_Init(SDL_INIT_VIDEO);
    SDL_CreateWindowAndRenderer(SDL_WINDOW_WIDTH, SDL_WINDOW_WIDTH, SDL_WINDOW_ALWAYS_ON_TOP, &sdl_window, &sdl_renderer);
    SDL_SetRenderDrawBlendMode(sdl_renderer,SDL_BLENDMODE_BLEND);
    SDL_SetRenderDrawColor(sdl_renderer, 0, 0, 0, 0);
    SDL_RenderClear(sdl_renderer);
    //Rendering directly into SDL's back buffer is not a good idea because double buffering gets in the way.
    //Instead we incrementally render the VGA output onto an SDL texture object and at regular intervals we copy the texture
    //object to SDL's back buffer.
    sdl_display = SDL_CreateTexture(sdl_renderer, SDL_PIXELFORMAT_RGBA8888, SDL_TEXTUREACCESS_TARGET, SDL_WINDOW_WIDTH, SDL_WINDOW_WIDTH);
    SDL_SetRenderTarget(sdl_renderer, sdl_display);

    //Trace file
    if (tracing_enable) {
      top->trace(tfp, 99); //Trace 99 levels deep.
      tfp->open("simx.fst");
    }
    
    // Set Vtop's input signals
    
    // External bus interface
    top->extbus_cs_n = !0;   /* Chip select */
    top->extbus_rd_n = !0;   /* Read strobe */
    top->extbus_wr_n = !0;   /* Write strobe */
    top->extbus_a = 0;      /* Address */
    top->extbus_d = 0;      /* Data (bi-directional) */

    top->clk25 = 0;
    contextp->timeInc(1);  // 1 timeprecision period passes...
    top->eval();

    //Let the synchronizer do its work. Spin until we come out of reset.
    while(contextp->time() < 1000) {
      tick();
    }

    //If a vram.bin file is given, load it into memory and poke it into VERA's VRAM
    if (vram_bin_filename) {
      if (load_bin_file_into_vram(vram_bin_filename) < 0)
        exit(-1);
    }

    //Fill VRAM map area with all characters
    for (int ii=0; ii<128*128/2; ii++) {
        vram_wr(VRAM_MAP_BASE+ii*2, ii&0xff);
        vram_wr(VRAM_MAP_BASE+ii*2+1, 0x01);
      }

    ext_bus_wr(VERA_DC_VIDEO, 0x11); //sprite disable, Layer 1 disable, Layer 0 enable, VGA output mode.
    ext_bus_wr(VERA_L0_CONFIG, 0xc0); //map size 128x128, tile mode, 1bpp.
    ext_bus_wr(VERA_L0_TILEBASE, 0x2); //tile base address 0, tile height/width 16x8.
    ext_bus_wr(VERA_L0_MAPBASE, VRAM_MAP_BASE>>9); //Map base address 0x10000

    //Curses setup
    initscr();
    cbreak();
    noecho();

    bool vsync_prev = false; 
    bool hsync_prev = false;

    //Wait for Vsync before starting the rendering
    while (!top->vga_vsync)
      tick(); //Advance one clock period.
      
    // When not in interactive mode, simulate for 10000000 timeprecision periods
    while (interactive_mode || (contextp->time() < 10000000)) {
        // Evaluate model
        tick();

        //Clear the screen during Vsync
        if (top->vga_vsync && !vsync_prev) {
          SDL_SetRenderDrawColor(sdl_renderer, 0, 0, 0, 0);
          SDL_RenderClear(sdl_renderer);
          sdl_y = 0;
        }

        //Render to SDL's back buffer at each Hsync.
        if (top->vga_hsync && !hsync_prev) {
          SDL_SetRenderTarget(sdl_renderer, NULL);
          SDL_RenderCopy(sdl_renderer, sdl_display, NULL, NULL);
          SDL_RenderPresent(sdl_renderer);
          SDL_SetRenderTarget(sdl_renderer, sdl_display);
          sdl_x = 0;
          sdl_y++;
        }

        //Render the VGA rgb output. Convert RGB4:4:4 to RGB8:8:8.
        SDL_SetRenderDrawColor(sdl_renderer, (Uint8)(top->vga_r<<4), (Uint8)(top->vga_g<<4), (Uint8)(top->vga_b<<4), 255);
        SDL_RenderDrawPoint(sdl_renderer, sdl_x, sdl_y);
        
        ++sdl_x;

        //Exit if user closes the SDL window.
        if (SDL_PollEvent(&sdl_event) && sdl_event.type == SDL_QUIT)
          break;

        vsync_prev = top->vga_vsync;
        hsync_prev = top->vga_hsync;

        //Positional printing using ncurses.
	      //mvprintw(0, 0, "[%lld %d %d]", contextp->time(), sdl_x, sdl_y);
	      //refresh();
    }

    //Close trace file.
    if (tracing_enable)
      tfp->close();
    
    // Final model cleanup
    top->final();

    // End curses.
    endwin();

    //SDL clean-up.
    SDL_DestroyRenderer(sdl_renderer);
    SDL_DestroyWindow(sdl_window);
    SDL_Quit();

    return 0;
}
