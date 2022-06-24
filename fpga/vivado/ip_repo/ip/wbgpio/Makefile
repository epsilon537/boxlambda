
.PHONY: package
package:
	 vivado -nolog -nojournal -mode batch -source gen_package.tcl 

.PHONY: synth
synth:
	 vivado -nolog -nojournal -mode batch -source gen_package.tcl -tclargs synth

.PHONY: clean
clean:
	$(RM) -rf project.* reports

