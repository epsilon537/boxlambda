#include "dfx_cli.h"
#include <assert.h>
#include <stdio.h>
#include "embedded_cli.h"
#include "dfx_controller_hal.h"
#include "vs0_hal.h"
#include "ff.h"
#include <stdlib.h>

#define DFX_LOAD_TIMEOUT_MS 3000

extern "C" {
  //CLI command to read the signature register of the core currently occupying Virtual Socket 0 (VS0).
  static void dfx_read_core_sig(EmbeddedCli *cli, char *args, void *context) {
    printf("Reading core signature register...\n");

    uint32_t sig = vs0_reg_rd(VS0_REG_SIGNATURE);

    printf("Read signature value: 0x%x\n", sig);
  }

  //CLI command to write to the DFX Control register.
  static void dfx_control(EmbeddedCli *cli, char *args, void *context) {
    if (embeddedCliGetTokenCount(args) < 3) {
        printf("Argument(s) missing: dfx_control <cmd> <extra byte> <extra halfword>.\n");
    }
    else {
      uint32_t cmd, extraByte, extraHalfword;
      dfx_ctrl_control_t dfx_control;

      char *cmdStr = embeddedCliGetTokenVariable(args, 1);
      char *extraByteStr = embeddedCliGetTokenVariable(args, 2);
      char *extraHalfwordStr = embeddedCliGetTokenVariable(args, 3);

      sscanf(cmdStr, "%d", &cmd);
      sscanf(extraByteStr, "%d", &extraByte);
      sscanf(extraHalfwordStr, "%d", &extraHalfword);

      printf("DFX_CONTROL: cmd:%d, extraByte: %d, extraHalfword: %d.\n", cmd, extraByte, extraHalfword);

      dfx_control.CMD = cmd;
      dfx_control.BYTE = extraByte;
      dfx_control.HALFWORD = extraHalfword;
      DFX_CTRL->CONTROL = dfx_control.UINT32;
    }
  }

  //CLI command to read the DFX status register.
  static void dfx_status(EmbeddedCli *cli, char *args, void *context) {
    dfx_other_status_t dfx_status;
    dfx_status.UINT32 = DFX_OTHER->STATUS;

    printf("DFX_STATUS: RM_ID: %d, Shutdown: %d, Err: %d, State: %d\n", dfx_status.RM_ID, dfx_status.SHUTDOWN, dfx_status.ERR, dfx_status.STATE);
  }

  //CLI command to read the DFX trigger register.
  static void dfx_trig_get(EmbeddedCli *cli, char *args, void *context) {
    dfx_other_sw_trigger_t sw_trigger;
    sw_trigger.UINT32 = DFX_OTHER->SW_TRIGGER;

    printf("SW TRIGGER: pending: %d, trigger_id: %d\n", sw_trigger.TRIGGER_PENDING, sw_trigger.TRIGGER_ID);

    uint32_t trig_0_reg = DFX_OTHER->TRIGGER_0;
    uint32_t trig_1_reg = DFX_OTHER->TRIGGER_1;

    printf("TRIGGER_0: %d, TRIGGER_1: %d\n", trig_0_reg, trig_1_reg);
  }

  //CLI command to write to the DFX trigger register.
  static void dfx_trig_set(EmbeddedCli *cli, char *args, void *context) {
    if (embeddedCliGetTokenCount(args) < 1) {
        printf("Argument(s) missing: dfx_trig_set <trig_id>\n");
    }
    else {
      uint32_t trig_id;

      char *token = embeddedCliGetTokenVariable(args, 1);
      sscanf(token, "%d", &trig_id);

      DFX_OTHER->SW_TRIGGER_bf.TRIGGER_ID = trig_id;

      dfx_other_sw_trigger_t sw_trigger;
      sw_trigger.UINT32 = DFX_OTHER->SW_TRIGGER;

      printf("SW TRIGGER: pending: %d, trigger_id: %d\n", sw_trigger.TRIGGER_PENDING, sw_trigger.TRIGGER_ID);
    }
  }

  //CLI command to read the DFX RM info register.
  static void dfx_rm_info_get(EmbeddedCli *cli, char *args, void *context) {
    uint32_t rm_bs_idx_reg = DFX_OTHER->RM_BS_INDEX_0;
    dfx_other_rm_control_0_t rm_control_0;
    rm_control_0.UINT32 = DFX_OTHER->RM_CONTROL_0;

    printf("RM0: BS_IDX: %d, rst_duration: %d, rst_required: %d, startup_required:%d, shutdown_required: %d.\n",
           rm_bs_idx_reg, rm_control_0.RST_DURATION, rm_control_0.RST_REQUIRED, rm_control_0.STARTUP_REQUIRED, rm_control_0.SHUTDOWN_REQUIRED);

    rm_bs_idx_reg = DFX_OTHER->RM_BS_INDEX_1;
    dfx_other_rm_control_1_t rm_control_1;
    rm_control_1.UINT32 = DFX_OTHER->RM_CONTROL_1;

    printf("RM1: BS_IDX: %d, rst_duration: %d, rst_required: %d, startup_required:%d, shutdown_required: %d.\n",
           rm_bs_idx_reg, rm_control_1.RST_DURATION, rm_control_1.RST_REQUIRED, rm_control_1.STARTUP_REQUIRED, rm_control_1.SHUTDOWN_REQUIRED);
  }

  //CLI command to read the DFX BS info register.
  static void dfx_bs_info_get(EmbeddedCli *cli, char *args, void *context) {
    uint32_t bs_addr = DFX_OTHER->BS_ADDRESS_0;
    uint32_t bs_size = DFX_OTHER->BS_SIZE_0;

    printf("BS0: addr: 0x%x, size: %d.\n", bs_addr, bs_size);

    bs_addr = DFX_OTHER->BS_ADDRESS_1;
    bs_size = DFX_OTHER->BS_SIZE_1;

    printf("BS1: addr: 0x%x, size: %d.\n", bs_addr, bs_size);
  }

  //CLI command to write to the DFX BS info register.
  static void dfx_bs_info_set(EmbeddedCli *cli, char *args, void *context) {
    if (embeddedCliGetTokenCount(args) < 3) {
        printf("Argument(s) missing: dfx_bs_info_set <idx> <hex addr> <size in bytes>\n");
    }
    else {
      uint32_t bs_idx, bs_adr, bs_size;

      char *bsIdxStr = embeddedCliGetTokenVariable(args, 1);
      char *bsAdrStr = embeddedCliGetTokenVariable(args, 2);
      char *bsSzStr = embeddedCliGetTokenVariable(args, 3);

      sscanf(bsIdxStr, "%d", &bs_idx);
      sscanf(bsAdrStr, "%08X", &bs_adr);
      sscanf(bsSzStr, "%d", &bs_size);

      if (bs_idx == 0) {
        DFX_OTHER->BS_ADDRESS_0 = bs_adr;
        DFX_OTHER->BS_SIZE_0 = bs_size;

        bs_adr = DFX_OTHER->BS_ADDRESS_0;
        bs_size = DFX_OTHER->BS_SIZE_0;

        printf("BS0: addr: 0x%x, size: %d.\n", bs_adr, bs_size);
      }
      else {
        DFX_OTHER->BS_ADDRESS_1 = bs_adr;
        DFX_OTHER->BS_SIZE_1 = bs_size;

        bs_adr = DFX_OTHER->BS_ADDRESS_1;
        bs_size = DFX_OTHER->BS_SIZE_1;

        printf("BS1: addr: 0x%x, size: %d.\n", bs_adr, bs_size);
      }
    }
  }

  /* CLI command to DFX load a reconfigurable module from disk into Virtual
   * Socket 0 (VS0). This function combines a number of register level
   * interactions with the DFX controller. */
  static void dfx_load_module(EmbeddedCli *cli, char *args, void *context) {

    uint16_t tokenCount = embeddedCliGetTokenCount(args);
    if (tokenCount < 1) {
        printf("Argument missing: dfx_load_module <filename>\n");
    }
    else {
      const char *filename = embeddedCliGetToken(args, 1);
	    FIL file_object;
      uint32_t addr;
      uint32_t size;

      /* Open the file */
      FRESULT res = f_open(&file_object, (char const *)filename, FA_OPEN_EXISTING | FA_READ);
      if (res != FR_OK) {
        printf("FatFS file open error! Error code: %d\n", res);
        return;
      }

      /* Allocate a memory buffer to store the file's contents. */
      size = f_size(&file_object);
      addr = (uint32_t)malloc(size);
      assert(addr);

      printf("Loading file %s, size: %d bytes, into memory at address 0x%x.\n", filename, size, addr);

      /* Read file */
      UINT bytes_read;
      res = f_read(&file_object, (void*)addr, size, &bytes_read);
      if (res != FR_OK) {
        printf("FatFS file read error! Error code: %d\n", res);
        return;
      }

      /* Close the file*/
      f_close(&file_object);

      /* Install the RM*/
      printf("Installing RM...\n");

      uint32_t dfx_res = dfx_load_rm((void*)addr, size, DFX_LOAD_TIMEOUT_MS);

      /* Release the memory buffer */
      free((void*)addr);

      if (dfx_res != 0) {
        printf("DFX Load RM failed. Error code %d.\n", dfx_res);
        return;
      }

      printf("Done.\n");
    }
  }
}

//Call this function to hooked the above functions in the embedded-CLI instance running on the system.
void add_dfx_cli(EmbeddedCli* cli) {
  assert(cli);

  embeddedCliAddBinding(cli, {
        "dfx_control",          // command name (spaces are not allowed)
        "dfx_control <cmd> <extra byte> <extra halfword> : Write to DFX Control Register.",   // Optional help for a command (NULL for no help)
        true,              // flag whether to tokenize arguments (see below)
        nullptr,            // optional pointer to any application context
        dfx_control               // binding function
  });

  embeddedCliAddBinding(cli, {
        "dfx_status",          // command name (spaces are not allowed)
        "Retrieve DFX status.",   // Optional help for a command (NULL for no help)
        true,              // flag whether to tokenize arguments (see below)
        nullptr,            // optional pointer to any application context
        dfx_status               // binding function
  });

  embeddedCliAddBinding(cli, {
        "dfx_trig_get",          // command name (spaces are not allowed)
        "Read DFX Trigger registers.",   // Optional help for a command (NULL for no help)
        true,              // flag whether to tokenize arguments (see below)
        nullptr,            // optional pointer to any application context
        dfx_trig_get               // binding function
  });

  embeddedCliAddBinding(cli, {
        "dfx_trig_set",          // command name (spaces are not allowed)
        "dfx_trig_set <trig_id>",   // Optional help for a command (NULL for no help)
        true,              // flag whether to tokenize arguments (see below)
        nullptr,            // optional pointer to any application context
        dfx_trig_set               // binding function
  });

  embeddedCliAddBinding(cli, {
        "dfx_rm_info_get",          // command name (spaces are not allowed)
        "Get Reconfigurable Module Info",   // Optional help for a command (NULL for no help)
        true,              // flag whether to tokenize arguments (see below)
        nullptr,            // optional pointer to any application context
        dfx_rm_info_get               // binding function
  });

  embeddedCliAddBinding(cli, {
        "dfx_bs_info_get",          // command name (spaces are not allowed)
        "Get Bitstream Info",   // Optional help for a command (NULL for no help)
        true,              // flag whether to tokenize arguments (see below)
        nullptr,            // optional pointer to any application context
        dfx_bs_info_get               // binding function
  });

  embeddedCliAddBinding(cli, {
        "dfx_bs_info_set",          // command name (spaces are not allowed)
        "dfx_bs_info_set <idx> <hex address> <size in bytes>",   // Optional help for a command (NULL for no help)
        true,              // flag whether to tokenize arguments (see below)
        nullptr,            // optional pointer to any application context
        dfx_bs_info_set               // binding function
  });

  embeddedCliAddBinding(cli, {
        "dfx_read_core_sig",          // command name (spaces are not allowed)
        "Read core's signature register",   // Optional help for a command (NULL for no help)
        true,              // flag whether to tokenize arguments (see below)
        nullptr,            // optional pointer to any application context
        dfx_read_core_sig  // binding function
  });

  embeddedCliAddBinding(cli, {
        "dfx_load_module",          // command name (spaces are not allowed)
        "dfx_load_module <filename>",   // Optional help for a command (NULL for no help)
        true,              // flag whether to tokenize arguments (see below)
        nullptr,            // optional pointer to any application context
        dfx_load_module  // binding function
  });
}

