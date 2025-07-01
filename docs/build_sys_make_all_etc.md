---
hide:
  - toc
---

Make All, Cgen, and Regen
-------------------------
In the `sim-a7-100` build tree, `make all` will lint check and build all `\*_sim` targets.

In the `arty-a7-100` build tree, `make all` will build `boxlambda_base_bit`, `boxlambda_dfx_bit`, `vs0_stub_bit`, and `vs0_j1b_bit`.

`Make cgen` in a build tree will run all code generation rules in that build tree. The register map, the ibex code base, LiteDRAM, and early bootstrap code rely on code generation. `Make cgen` must be run whenever there are changes in those parts of the system.

If you want to go back to a clean build tree, type `make regen` from the build directory. This command will:

1. remove and regenerate the build tree.
2. run the code generation rules (`make cgen`).

