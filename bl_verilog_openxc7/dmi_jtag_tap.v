`default_nettype none
module dmi_jtag_tap (
	tck_i,
	tms_i,
	trst_ni,
	td_i,
	td_o,
	tdo_oe_o,
	testmode_i,
	tck_o,
	dmi_clear_o,
	update_o,
	capture_o,
	shift_o,
	tdi_o,
	dtmcs_select_o,
	dtmcs_tdo_i,
	dmi_select_o,
	dmi_tdo_i
);
	parameter [31:0] IrLength = 5;
	parameter [31:0] IdcodeValue = 32'h00000001;
	input wire tck_i;
	input wire tms_i;
	input wire trst_ni;
	input wire td_i;
	output wire td_o;
	output wire tdo_oe_o;
	input wire testmode_i;
	output wire tck_o;
	output wire dmi_clear_o;
	output wire update_o;
	output wire capture_o;
	output wire shift_o;
	output wire tdi_o;
	output wire dtmcs_select_o;
	input wire dtmcs_tdo_i;
	output wire dmi_select_o;
	input wire dmi_tdo_i;
	BSCANE2 #(.JTAG_CHAIN(3)) i_tap_dtmcs(
		.CAPTURE(capture_o),
		.DRCK(),
		.RESET(dmi_clear_o),
		.RUNTEST(),
		.SEL(dtmcs_select_o),
		.SHIFT(shift_o),
		.TCK(tck_o),
		.TDI(tdi_o),
		.TMS(),
		.TDO(dtmcs_tdo_i),
		.UPDATE(update_o)
	);
	BSCANE2 #(.JTAG_CHAIN(4)) i_tap_dmi(
		.CAPTURE(),
		.DRCK(),
		.RESET(),
		.RUNTEST(),
		.SEL(dmi_select_o),
		.SHIFT(),
		.TCK(),
		.TDI(),
		.TMS(),
		.TDO(dmi_tdo_i),
		.UPDATE()
	);
endmodule
