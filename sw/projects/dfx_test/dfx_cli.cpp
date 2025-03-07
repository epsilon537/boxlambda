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

      char *cmdStr = embeddedCliGetTokenVariable(args, 1);
      char *extraByteStr = embeddedCliGetTokenVariable(args, 2);
      char *extraHalfwordStr = embeddedCliGetTokenVariable(args, 3);

      sscanf(cmdStr, "%d", &cmd);
      sscanf(extraByteStr, "%d", &extraByte);
      sscanf(extraHalfwordStr, "%d", &extraHalfword);

      printf("DFX_CONTROL: cmd:%d, extraByte: %d, extraHalfword: %d.\n", cmd, extraByte, extraHalfword);

      cmd &= DFX_CONTROL_CMD_MASK;

      extraByte <<= DFX_CONTROL_BYTE_OFFSET;
      extraByte &= DFX_CONTROL_BYTE_MASK;

      extraHalfword <<= DFX_CONTROL_HALFWORD_OFFSET;
      extraHalfword &= DFX_CONTROL_HALFWORD_MASK;

      dfx_reg_wr(DFX_CONTROL_REG, extraHalfword | extraByte | cmd);
    }
  }

  //CLI command to read the DFX status register.
  static void dfx_status(EmbeddedCli *cli, char *args, void *context) {
    uint32_t status_reg = dfx_reg_rd(DFX_STATUS_REG);
    uint32_t rm_id = (status_reg & DFX_STATUS_RM_ID_MASK)>>DFX_STATUS_RM_ID_OFFSET;
    uint32_t shutdown = (status_reg & DFX_STATUS_SHUTDOWN) ? 1 : 0;
    uint32_t err = (status_reg & DFX_STATUS_ERR_MASK) >> DFX_STATUS_ERR_OFFSET;
    uint32_t state = (status_reg & DFX_STATUS_STATE_MASK);

    printf("DFX_STATUS: RM_ID: %d, Shutdown: %d, Err: %d, State: %d\n", rm_id, shutdown, err, state);
  }

  //CLI command to read the DFX trigger register.
  static void dfx_trig_get(EmbeddedCli *cli, char *args, void *context) {
    uint32_t sw_trig_reg = dfx_reg_rd(DFX_SW_TRIGGER_REG);
    uint32_t sw_trig_pending = (sw_trig_reg & DFX_SW_TRIGGER_PENDING) ? 1 : 0;
    uint32_t sw_trig_id = (sw_trig_reg & DFX_SW_TRIGGER_TRIG_ID);

    printf("SW TRIGGER: pending: %d, trigger_id: %d\n", sw_trig_pending, sw_trig_id);

    uint32_t trig_0_reg = dfx_reg_rd(DFX_TRIGGER_0_REG);
    uint32_t trig_1_reg = dfx_reg_rd(DFX_TRIGGER_1_REG);

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

      dfx_reg_wr(DFX_SW_TRIGGER_REG, trig_id & DFX_SW_TRIGGER_TRIG_ID);

      uint32_t sw_trig_reg = dfx_reg_rd(DFX_SW_TRIGGER_REG);
      uint32_t sw_trig_pending = (sw_trig_reg & DFX_SW_TRIGGER_PENDING) ? 1 : 0;
      uint32_t sw_trig_id = (sw_trig_reg & DFX_SW_TRIGGER_TRIG_ID);

      printf("SW TRIGGER: pending: %d, trigger_id: %d\n", sw_trig_pending, sw_trig_id);
    }
  }

  //CLI command to read the DFX RM info register.
  static void dfx_rm_info_get(EmbeddedCli *cli, char *args, void *context) {
    uint32_t rm_bs_idx_reg = dfx_reg_rd(DFX_RM_BS_INDEX_0_REG);
    uint32_t rm_ctrl_reg = dfx_reg_rd(DFX_RM_CONTROL_0_REG);
    uint32_t rst_duration = (rm_ctrl_reg & DFX_RM_CONTROL_RST_DURATION_MASK) >> DFX_RM_CONTROL_RST_DURATION_OFFSET;
    uint32_t rst_required = (rm_ctrl_reg & DFX_RM_CONTROL_RST_REQUIRED_MASK) >> DFX_RM_CONTROL_RST_REQUIRED_OFFSET;
    uint32_t startup_required = (rm_ctrl_reg & DFX_RM_CONTROL_STARTUP_REQUIRED) ? 1 : 0;
    uint32_t shutdown_required = (rm_ctrl_reg & DFX_RM_CONTROL_SHUTDOWN_REQUIRED_MASK);

    printf("RM0: BS_IDX: %d, rst_duration: %d, rst_required: %d, startup_required:%d, shutdown_required: %d.\n",
           rm_bs_idx_reg, rst_duration, rst_required, startup_required, shutdown_required);

    rm_bs_idx_reg = dfx_reg_rd(DFX_RM_BS_INDEX_1_REG);
    rm_ctrl_reg = dfx_reg_rd(DFX_RM_CONTROL_1_REG);
    rst_duration = (rm_ctrl_reg & DFX_RM_CONTROL_RST_DURATION_MASK) >> DFX_RM_CONTROL_RST_DURATION_OFFSET;
    rst_required = (rm_ctrl_reg & DFX_RM_CONTROL_RST_REQUIRED_MASK) >> DFX_RM_CONTROL_RST_REQUIRED_OFFSET;
    startup_required = (rm_ctrl_reg & DFX_RM_CONTROL_STARTUP_REQUIRED) ? 1 : 0;
    shutdown_required = (rm_ctrl_reg & DFX_RM_CONTROL_SHUTDOWN_REQUIRED_MASK);

    printf("RM1: BS_IDX: %d, rst_duration: %d, rst_required: %d, startup_required:%d, shutdown_required: %d.\n",
           rm_bs_idx_reg, rst_duration, rst_required, startup_required, shutdown_required);
  }

  //CLI command to read the DFX BS info register.
  static void dfx_bs_info_get(EmbeddedCli *cli, char *args, void *context) {
    uint32_t bs_addr = dfx_reg_rd(DFX_BS_ADDRESS_0_REG);
    uint32_t bs_size = dfx_reg_rd(DFX_BS_SIZE_0_REG);

    printf("BS0: addr: 0x%x, size: %d.\n", bs_addr, bs_size);

    bs_addr = dfx_reg_rd(DFX_BS_ADDRESS_1_REG);
    bs_size = dfx_reg_rd(DFX_BS_SIZE_1_REG);

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
        dfx_reg_wr(DFX_BS_ADDRESS_0_REG, bs_adr);
        dfx_reg_wr(DFX_BS_SIZE_0_REG, bs_size);

        bs_adr = dfx_reg_rd(DFX_BS_ADDRESS_0_REG);
        bs_size = dfx_reg_rd(DFX_BS_SIZE_0_REG);

        printf("BS0: addr: 0x%x, size: %d.\n", bs_adr, bs_size);
      }
      else {
        dfx_reg_wr(DFX_BS_ADDRESS_1_REG, bs_adr);
        dfx_reg_wr(DFX_BS_SIZE_1_REG, bs_size);

        bs_adr = dfx_reg_rd(DFX_BS_ADDRESS_1_REG);
        bs_size = dfx_reg_rd(DFX_BS_SIZE_1_REG);

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

