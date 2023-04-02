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
#define VRAM_MAP_BASE (0x10000|VERA_VRAM_BASE)

#define WB_ACK_TIMEOUT 100

//SDL objects:
#define SDL_WINDOW_WIDTH 800

SDL_Event sdl_event;
SDL_Renderer *sdl_renderer;
SDL_Window *sdl_window;
SDL_Texture *sdl_display;

int sdl_x=0/*750*/, sdl_y=0/*523*/;

bool vsync_prev = false; 
bool hsync_prev = false;
bool exit_req = false;
bool render = false;

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

void cleanup() {
  //SDL clean-up.
  SDL_DestroyRenderer(sdl_renderer);
  SDL_DestroyWindow(sdl_window);
  SDL_Quit();

  //Close trace file.
  if (tracing_enable)
    tfp->close();

  // Final model cleanup
  top->final();
}

//Advance simulation by one clock cycle
static void tick(void) {
  top->clk = 1;
  contextp->timeInc(1);
  top->eval();
  if (tracing_enable)
    tfp->dump(contextp->time());
  top->clk = 0;
  contextp->timeInc(1);
  top->eval();
  if (tracing_enable)
    tfp->dump(contextp->time());

  if (!render)
    return;

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
  SDL_RenderDrawPoint(sdl_renderer, sdl_x>>1, sdl_y);
  
  ++sdl_x;

  //Exit if user closes the SDL window.
  if (SDL_PollEvent(&sdl_event) && sdl_event.type == SDL_QUIT)
    exit_req = true;

  vsync_prev = top->vga_vsync;
  hsync_prev = top->vga_hsync;
}

//A very crude wishbone bus write implementation.
void wb_wr(unsigned addr, unsigned data) {
  top->wb_adr = addr>>2;      
  top->wb_dat_w = data;    

  top->wb_cyc = 1;
  top->wb_stb = 1;  
  top->wb_we = 1;
  top->wb_sel = 0xf;

  while (!top->wb_ack)
    tick();

  top->wb_cyc = 0;
  top->wb_stb = 0;  
  top->wb_we = 0;
  top->wb_sel = 0xf;

  tick();
  tick();
}

//A very crude wishbone bus read implementation.
int wb_rd(unsigned addr, unsigned &data) {
  unsigned char res;

  top->wb_adr = addr>>2;
  top->wb_cyc = 1;
  top->wb_stb = 1;  
  top->wb_we = 0;
  
  int timeout_counter=0;

  while (!top->wb_ack && (timeout_counter++ < WB_ACK_TIMEOUT))
    tick();

  if (timeout_counter >= WB_ACK_TIMEOUT) {
    printf("wb_ack timeout!\r\n");
    res = -1;
  }
  else {
    data = top->wb_dat_r;
    res = 0;
  }

  top->wb_cyc = 0;
  top->wb_stb = 0;  
  top->wb_we = 0;

  tick();
  
  return res;
}

//This function writes the given data word to the given address in VERA's VRAM.
void vram_wr(unsigned addr, unsigned data) {
  top->wb_adr = (addr | VERA_VRAM_BASE)>>2;      
  top->wb_dat_w = data;    

  top->wb_cyc = 1;
  top->wb_stb = 1;  
  top->wb_we = 1;
  top->wb_sel = 0xf;

  while (!top->wb_ack)
    tick();

  top->wb_cyc = 0;
  top->wb_stb = 0;  
  top->wb_we = 0;
  top->wb_sel = 0;

  tick();
}

//This function writes the given data byte to the given address in VERA's VRAM.
void vram_wr_byte(unsigned addr, unsigned char data) {
  unsigned addr_aligned = addr & (~3);
  unsigned byte_shift = (addr-addr_aligned);
  unsigned byte_enable = 1<<byte_shift;

  top->wb_adr = (addr_aligned | VERA_VRAM_BASE)>>2;      
  top->wb_dat_w = ((unsigned)data)<<(byte_shift*8);    

  top->wb_cyc = 1;
  top->wb_stb = 1;  
  top->wb_we  = 1;
  top->wb_sel = byte_enable;

  while (!top->wb_ack)
    tick();

  top->wb_cyc = 0;
  top->wb_stb = 0;  
  top->wb_we = 0;
  top->wb_sel = 0;

  tick();
}

int vram_rd(unsigned addr, unsigned& data) {
  int res=0;

  top->wb_adr = (addr | VERA_VRAM_BASE)>>2;      

  top->wb_cyc = 1;
  top->wb_stb = 1;  
  top->wb_we = 0;
  top->wb_sel = 0xf;

  int timeout_counter=0;

  while (!top->wb_ack && (timeout_counter++ < WB_ACK_TIMEOUT))
    tick();

  if (timeout_counter >= WB_ACK_TIMEOUT) {
    printf("wb_ack timeout!\r\n");
    res = -1;
  }
  else {
    data = top->wb_dat_r;
    res = 0;
  }

  top->wb_cyc = 0;
  top->wb_stb = 0;  
  top->wb_we = 0;
  top->wb_sel = 0;

  tick();

  return res;
}

int vram_rd_byte(unsigned addr, unsigned char& data) {
  int res=0;
  unsigned addr_aligned = addr & (~3);
  unsigned byte_shift = (addr-addr_aligned);
  unsigned byte_enable = 1<<byte_shift;

  top->wb_adr = (addr_aligned | VERA_VRAM_BASE)>>2;      

  top->wb_cyc = 1;
  top->wb_stb = 1;  
  top->wb_we = 0;
  top->wb_sel = byte_enable;

  int timeout_counter=0;

  while (!top->wb_ack && (timeout_counter++ < WB_ACK_TIMEOUT))
    tick();

  if (timeout_counter >= WB_ACK_TIMEOUT) {
    printf("wb_ack timeout!\r\n");
    res = -1;
  }
  else {
    data = (unsigned char)(top->wb_dat_r>>(byte_shift*8));
    res = 0;
  }

  top->wb_cyc = 0;
  top->wb_stb = 0;  
  top->wb_we = 0;
  top->wb_sel = 0;

  tick();

  return res;
}

//This function writes the given rgb triple to the given position in VERA's Palette RAM.
void palette_ram_wr(unsigned idx, unsigned char r, unsigned char g, unsigned char b) {
  top->wb_adr = ((idx<<2) | VERA_PALETTE_BASE)>>2;      
  top->wb_dat_w = (((unsigned)r)<<8) | (((unsigned)g)<<4) | ((unsigned)b);    

  top->wb_cyc = 1;
  top->wb_stb = 1;  
  top->wb_we = 1;
  top->wb_sel = 0x3;

  while (!top->wb_ack)
    tick();

  top->wb_cyc = 0;
  top->wb_stb = 0;  
  top->wb_we = 0;
  top->wb_sel = 0;

  tick();
}

void setup_palette_ram(void) {
  for (unsigned ii=0; ii<256; ii++) {
    palette_ram_wr(ii, ((ii>>4)&3)<<2, ((ii>>2)&3)<<2, (ii&3)<<2);
  }
}

//This function writes the given data word to the given address in VERA's Sprite RAM.
void sprite_ram_wr(unsigned addr, unsigned data) {
  top->wb_adr = (addr | VERA_SPRITES_BASE)>>2;      
  top->wb_dat_w = data;    

  top->wb_cyc = 1;
  top->wb_stb = 1;  
  top->wb_we = 1;
  top->wb_sel = 0xf;

  while (!top->wb_ack)
    tick();

  top->wb_cyc = 0;
  top->wb_stb = 0;  
  top->wb_we = 0;
  top->wb_sel = 0;

  tick();
}

void setup_sprite_ram() {
  int i;
  unsigned v,w;

  for (i=0; i<64; i++) {
    v = (0x1c0>>5); // addr
    v |= (1<<15); // mode: 8bpp
    v |= ((8*i)<<16); //x
    w = 16; //y
    w |= (3<<18); //z
    //width:8
    //height:8

    sprite_ram_wr(i*8, v);
    sprite_ram_wr(i*8 + 4, w);
  }

  for (i=0; i<64; i++) {
    v = (0x1000>>5); // addr
    v |= (1<<15); // mode: 8bpp
    v |= ((70*i)<<16); //x
    w = 300; //y
    w |= (3<<18); //z
    w |= (3<<28); //width:64
    w |= (3<<30); //heigth

    sprite_ram_wr(64*8 + i*8, v);
    sprite_ram_wr(64*8 + i*8 + 4, w);
  }
}

//Returns <0 if unsuccessful
int load_bin_file_into_vram(const char* vram_bin_filename) {
  static unsigned char buffer[VRAM_SIZE_BYTES];
  int n;

  printf("Loading into VRAM: %s\n\r", vram_bin_filename);

  FILE *f = fopen(vram_bin_filename, "rb");
  if (f) {
    n = fread(buffer, 1, VRAM_SIZE_BYTES, f);

    for (int ii=0; ii<n/4; ii++) {
      vram_wr(ii*4, 
        (unsigned)(buffer[ii*4+3]<<24)|(unsigned)(buffer[ii*4+2]<<16)|(unsigned)(buffer[ii*4+2]<<8)|(unsigned)buffer[ii*4]);
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

void generate_8bpp_8x8_tiles() {
  unsigned char data=0;

  //Just generate 8x8 blocks of different colors
  for (int jj=0;jj<16;jj++) {
    for (int ii=0; ii<64; ii++) {
      vram_wr_byte(jj*64+ii, (ii%8 >= 4) ? jj : 0);

      if (vram_rd_byte(jj*64+ii, data) < 0) {
        printf("VRAM read ack timeout.\n\r");
        cleanup();
        exit(-1);
      }

      if (data != ((ii%8 >= 4) ? jj : 0)) {
        printf("VRAM read back mismatch addr: 0x%x: 0x%x vs. 0x%x.\n\r", jj*64+ii, data, ((ii%8 >= 4) ? jj : 0));
        cleanup();
        exit(-1);
      }
    }
  }

  printf("VRAM readback OK.\n\r");
}

void generate_8bpp_64x64_sprite() {
  for (int ii=0; ii<64*64/4; ii++) {
      vram_wr(0x1000+ii*4, 0x03030303);
    } 
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
    top->wb_cyc = 0;
    top->wb_we = 0;
    top->wb_stb = 0;
    top->wb_adr = 0;   
    top->wb_dat_w = 0;    

    top->reset = 1;
    tick();
    tick();
    tick();
    top->reset = 0;

#if 0
    //If a vram.bin file is given, load it into memory and poke it into VERA's VRAM
    if (vram_bin_filename) {
      if (load_bin_file_into_vram(vram_bin_filename) < 0)
        exit(-1);
    }
#endif

    generate_8bpp_8x8_tiles();
    generate_8bpp_64x64_sprite();
    setup_sprite_ram();
    setup_palette_ram();
    
    //Fill VRAM map area
    for (int ii=0; ii<128*128*2; ii+=2) {
      vram_wr_byte(VRAM_MAP_BASE+ii, (unsigned char)ii&0xf);
      vram_wr_byte(VRAM_MAP_BASE+ii+1, 0);
    }

    unsigned read_back_val=0;

    wb_wr(VERA_DC_VIDEO, 0x71); //sprite enable, Layer 1 enable, Layer 0 enable, VGA output mode.
    if (wb_rd(VERA_DC_VIDEO, read_back_val) < 0) {
      printf("VERA_DC_VIDEO read back failed.\n\r");
      cleanup();
      exit(-1);
    }

    if (read_back_val != 0x71) {
      printf("VERA_DC_VIDEO read back incorrectly: 0x%x\n\r", read_back_val);
      cleanup();
      exit(-1);
    }
    else {
      printf("VERA_DC_VIDEO read back OK\n\r");
    }

    wb_wr(VERA_L0_CONFIG, 0xc3); //map size 128x128, tile mode, 8bpp.
    wb_wr(VERA_L0_TILEBASE, 0x0); //tile base address 0, tile height/width 8x8.
    wb_wr(VERA_L0_MAPBASE, VRAM_MAP_BASE>>9); //Map base address 0x10000
    wb_wr(VERA_L1_CONFIG, 0xc3); //map size 128x128, tile mode, 8bpp.
    wb_wr(VERA_L1_TILEBASE, 0x0); //tile base address 0, tile height/width 8x8.
    wb_wr(VERA_L1_MAPBASE, VRAM_MAP_BASE>>9); //Map base address 0x10000
    wb_wr(VERA_CTRL, 0); //Sprite Bank 0

    //Curses setup
    initscr();
    cbreak();
    noecho();

    //Wait for Vsync before starting the rendering
    while (!top->vga_vsync)
      tick(); //Advance one clock period.

    render = true;

    // When not in interactive mode, simulate for 100000000 timeprecision periods
    while (interactive_mode || (contextp->time() < 100000000)) {
        if (exit_req)
          break;

        // Evaluate model
        tick();
        
        //if (contextp->time() % 10 == 0)
          vram_wr(0,0); //Stress the bus by writing all the time.

        if (sdl_y == 1) {
          wb_wr(VERA_CTRL, 0); //Sprite Bank 0
        }

        if (sdl_y == 240) {
          wb_wr(VERA_CTRL, 1); //Sprite Bank 1
        }

        //Positional printing using ncurses.
	      //mvprintw(0, 0, "[%lld %d %d]", contextp->time(), sdl_x, sdl_y);
	      //refresh();
    }
    
    cleanup();

    // End curses.
    endwin();

    return 0;
}
