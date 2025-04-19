---
hide:
  - toc
---

Make All, Clean, and Regen
--------------------------
In the `sim-a7-100` build tree, *make all* will lint check and build all *\*_sim* targets.

In the `arty-a7-100` build tree, *make all* will build `boxlambda_base_bit`, `boxlambda_dfx_bit`, `vs0_stub_bit`, and `vs0_j1b_bit`. 

`Make clean` in a build tree will remove all the generated files that the build system is aware of. The generated files the build system is not aware of, e.g. synthesis utilization report files, will not be removed, however. If you want to go back to a completely clean build tree, type `make regen` from the build directory. This command will completely remove and regenerate the build tree.
