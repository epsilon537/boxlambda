---
hide:
  - toc
---

Make All, Clean, and Regen
--------------------------
*make all* will lint check and build **bit_sw/sim_sw** all gateware projects.

*Make clean* in a build tree will remove all the generated files that the build system is aware of. The generated files the build system is not aware of, e.g. synthesis utilization report files, will not be removed, however. If you want to go back to a completely clean build tree, type *make regen* from the build directory. This command will completely remove and regenerate the build tree.
