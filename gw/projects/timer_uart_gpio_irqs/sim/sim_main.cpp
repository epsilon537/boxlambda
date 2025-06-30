#include <getopt.h>
#include <fcntl.h>

// For std::unique_ptr
#include <memory>

#include <string>
#include <stdio.h>

// Include common routines
#include <verilated.h>

// Include model header, generated from Verilating "top.v"
#include "Vmodel.h"

#include "verilated_fst_c.h"

//To get access to the verilated model internals.
#include "Vmodel___024root.h"

// From wbuart32
#include "uartsim.h"

// From riscv-dbg
#include "sim_jtag.h"

bool tracing_enable = false;

//Uart co-simulation from wbuart32.
std::unique_ptr<UARTSIM> uart{new UARTSIM(0)};

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

//Initialize UART rx and tx change detector
std::string uartRxStringPrev;

// Legacy function required only so linking works on Cygwin and MSVC++
double sc_time_stamp() { return 0; }

//Clean-up logic.
static void cleanup() {
  //Close trace file.
  if (tracing_enable)
    tfp->close();

  // Final model cleanup
  top->final();
}

//Advance simulation by one clock cycle
static void tick(unsigned gp_in) {
  //Tick twice: Input clock is 100MHz, BoxLambda's system clock runs at 50MHz.
  //->Advance two input clock cycles at a time.
  for (int ii=0; ii<2;ii++) {
    //High phase
    top->clk_i = 1;
    contextp->timeInc(1);
    top->eval();
    if (tracing_enable)
      tfp->dump(contextp->time());

    //Low phase
    top->clk_i = 0;
    contextp->timeInc(1);
    top->eval();
    if (tracing_enable)
      tfp->dump(contextp->time());
  }

  //The test sequence in main() drives the GPIO inputs via the gp_in variable
  top->gp_in = gp_in;

  //Feed our model's uart_tx signal and baud rate to the UART co-simulator.
  //and feed the UART co-simulator output to our model
  top->uart_rx = (*uart)(top->uart_tx,
    top->rootp->sim_main__DOT__dut__DOT__boxlambda_soc_inst__DOT__wbuart_inst__DOT__uart_setup);

  //Detect and print changes to UART
  if (uart->get_rx_string().back() == '\n')  {
    printf("SIM: DUT: %s", uart->get_rx_string().c_str());

    //Update change detectors
    uartRxStringPrev += uart->get_rx_string();

    uart->clear_rx_string();
  }
}

static int timer_test(void) {
  int done = 0;

  while (!done && (contextp->time() < 4000000)) {
    // Evaluate model
    tick(0 /*gp_in*/);

    std::string uartCheckString("Timer Test Successful.");

    if (uartRxStringPrev.find(uartCheckString) != std::string::npos)
      done = 1;
  }

  if (!done) {
    printf("SIM: Timer test failed.\n");
    return -1;
  }

  printf("SIM: SIM: Timer test successful. Time = %lu\n", contextp->time());

  return 0;
}

static int uart_tx_irq_test(void) {
  int done = 0;

  while (!done && (contextp->time() < 9000000)) {
    // Evaluate model
    tick(0 /*gp_in*/);

    std::string uartCheckString("UART TX IRQ test successful.");

    if (uartRxStringPrev.find(uartCheckString) != std::string::npos)
      done = 1;
  }

  if (!done) {
    printf("SIM: UART TX IRQ test failed.\n");
    return -1;
  }

  printf("SIM: SIM: UART TX IRQ test successful. Time = %lu\n", contextp->time());

  return 0;
}

static int uart_rx_irq_test_single(void) {
  int done = 0;

  while (!done && (contextp->time() < 7000000)) {
    // Evaluate model
    tick(0 /*gp_in*/);

    std::string uartCheckString("Please enter a character.");

    if (uartRxStringPrev.find(uartCheckString) != std::string::npos)
      done = 1;
  }

  if (!done) {
    printf("SIM: UART RX IRQ test failed (1).\n");
    return -1;
  }

  printf("SIM: SIM: inserting uart character (1). Time = %lu\n", contextp->time());
  //Enter a single character into the UART
  uart->enterCharInTxPath('a');

  /*
   * UART Rx IRQ test - single character - repeat
   */
  done = 0;

  while (!done && (contextp->time() < 8000000)) {
    // Evaluate model
    tick(0 /*gp_in*/);

    std::string uartCheckString("Please enter another character.");

    if (uartRxStringPrev.find(uartCheckString) != std::string::npos)
      done = 1;
  }

  if (!done) {
    printf("SIM: UART RX IRQ test failed (2).\n");
    return -1;
  }

  printf("SIM: SIM: inserting uart character (2). Time = %lu\n", contextp->time());
  //Enter a single character into the UART
  uart->enterCharInTxPath('b');

  return 0;
}

static int uart_rx_irq_test_seq(void) {
  int done = 0;

  while (!done && (contextp->time() < 10000000)) {
    // Evaluate model
    tick(0 /*gp_in*/);

    std::string uartCheckString("Please enter 8 characters. They will be echoed when all 8 characters are received.");

    if (uartRxStringPrev.find(uartCheckString) != std::string::npos)
      done = 1;
  }

  if (!done) {
    printf("SIM: UART RX IRQ test failed (3).\n");
    return -1;
  }

  printf("SIM: SIM: inserting character sequence. Time = %lu\n", contextp->time());

  done = 0;

  char strToSend[] = "Hello World!";
  char *strPtr = &strToSend[0];
  char *strPtrEnd = strPtr + strlen(strToSend);

  while (!done) {
    // Evaluate model
    tick(0 /*gp_in*/);

    //Enter character sequence into the UART
    if (uart->enterCharInTxPath(*strPtr) != -1) {
      if (strPtr == strPtrEnd) {
        printf("SIM: SIM: finished inserting uart string (1).\n");
        done = 1;
        strPtr = &strToSend[0]; //Reset strPtr to the beginning.
      }
      else {
        ++strPtr;
      }
    }
  }

  done = 0;

  while (!done && (contextp->time() < 18000000)) {
    // Evaluate model
    tick(0 /*gp_in*/);

    std::string uartCheckString("Please enter 8 characters again. They will be echoed when all 8 characters are received.");

    if (uartRxStringPrev.find(uartCheckString) != std::string::npos)
      done = 1;
  }

  if (!done) {
    printf("SIM: UART RX IRQ test failed (4).\n");
    return -1;
  }

  printf("SIM: SIM: Inserting character sequence again. Time = %lu\n", contextp->time());

  done = 0;

  while (!done) {
    // Evaluate model
    tick(0 /*gp_in*/);

    //Enter character sequence into the UART
    if (uart->enterCharInTxPath(*strPtr) != -1) {
      if (strPtr == strPtrEnd) {
        printf("SIM: SIM: finished inserting uart string (2).\n");
        done = 1;
        strPtr = &strToSend[0]; //Reset strPtr to the beginning.
      }
      else {
        ++strPtr;
      }
    }
  }

  done = 0;

  while (!done && (contextp->time() < 24000000)) {
    // Evaluate model
    tick(0 /*gp_in*/);

    std::string uartCheckString("UART RX IRQ Test Successful.");

    if (uartRxStringPrev.find(uartCheckString) != std::string::npos)
      done = 1;
  }

  if (!done) {
    printf("SIM: UART RX IRQ test failed (5).\n");
    return -1;
  }

  return 0;
}

int gpio_test(void) {
  int done = 0;
  unsigned gp_in = 0;

  while (!done && (contextp->time() < 25000000)) {
    // Evaluate model
    tick(gp_in);

    std::string uartCheckString("Push some buttons. The LEDS should track the button presses/releases.");

    if (uartRxStringPrev.find(uartCheckString) != std::string::npos)
      done = 1;
  }

  if (!done) {
    printf("SIM: GPIO test failed (1).\n");
    return -1;
  }

  printf("SIM: SIM: Testing GPIO. Time = %lu\n", contextp->time());

  done = 0;

  int button0pushed = 0, button1pushed = 0;
  int led0turnedOn = 0, led1turnedOn = 0;

  while (!done && (contextp->time() < 25000000)) {
    // Evaluate model
    tick(gp_in);

    //'Push' one button. When corresponding leds turns on, release the button.
    //When led turns off again, move on to the next button.
    if (!button0pushed) {
        gp_in = 0x100;     //Push button 0.
        button0pushed = 1; //Indicate that button 0 has been pushed.
    }

    if (!led0turnedOn && button0pushed && ((top->gp_out & 1) == 1) && ((top->gp_oe & 1) == 1)) {
      printf("SIM: SIM: LED 0 turned on.\n");

      gp_in = 0; //Release button 0 again.
      led0turnedOn = 1; //Indicate that LED 0 has been turned on.
    }

    if (led0turnedOn && !button1pushed && ((top->gp_out & 1) == 0) && ((top->gp_oe & 1) == 1)) {
       printf("SIM: SIM: LED 0 turned off.\n");

       gp_in = 0x200; //Push button 1
       button1pushed = 1; //Indicate that button 1 has been pushed.
    }

    if (!led1turnedOn && button1pushed && ((top->gp_out & 2) == 2) && ((top->gp_oe & 2) == 2)) {
      printf("SIM: SIM: LED 1 turned on.\n");

      gp_in = 0; //Release button again.
      led1turnedOn = 1; //Indicate that LED 1 has been turned on.
    }

    if (led1turnedOn && ((top->gp_out & 2) == 0) && ((top->gp_oe & 2) == 2)) {
      if (!done) {
        printf("SIM: SIM: LED 1 turned off.\n");
        printf("SIM: SIM: GPIO test OK.\n");
      }

      done = 1;
    }
  }

  if (!done) {
    printf("SIM: SIM: GPIO test failed.\n");
    return -1;
  }

  printf("SIM: SIM: GPIO test successful. Time = %lu\n", contextp->time());

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

    // Command line processing
    for(;;) {
      switch(getopt(argc, argv, "ath")) {
      case 'a':
        attach_debugger = true;
        continue;
      case 't':
        printf("SIM: Tracing enabled\n");
        tracing_enable = true;
        continue;
      case '?':
      case 'h':
      default :
        printf("SIM: \nVmodel Usage:\n");
        printf("SIM: -h: print this help\n");
        printf("SIM: -a: attach debugger.\n");
        printf("SIM: -t: enable tracing.\n");
        return 0;
        break;

      case -1:
        break;
      }

      break;
    }

    //Trace file
    if (tracing_enable) {
      top->trace(tfp, 99); //Trace 99 levels deep.
      tfp->open("simx.fst");
    }

    jtag_set_bypass(!attach_debugger);

    top->clk_i = 0;
    top->uart_rx = 0;

    //Take the system out of reset.
    top->rst_ni = 1;

    /* This sequence of loops trigger actions based on cues received from the DUT over the UART.*/

    /*
     * Timer Test
     */
    if (timer_test() != 0)
      return -1;

    /*
     * UART Tx IRQ test
     */
    if (uart_tx_irq_test() != 0)
      return -1;

    /*
     * UART Rx IRQ test - single character
     */
    if (uart_rx_irq_test_single() != 0)
      return -1;

    /*
     * UART Rx IRQ test - character sequence.
     */
    if (uart_rx_irq_test_seq() != 0)
      return -1;

    printf("SIM: SIM: UART RX IRQ tests successful. Time = %lu\n", contextp->time());

    /*
     * GPIO IRQ test - simulate push buttons and verify LED tracking the buttons being pushed.
     */
    if (gpio_test() != 0)
      return -1;

    printf("SIM: Test passed.\n");

    cleanup();

    return 0;
}
