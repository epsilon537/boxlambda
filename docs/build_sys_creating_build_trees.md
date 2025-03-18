---
hide:
  - toc
---

# Creating Additional Build Trees

You can create additional build trees easily from the BoxLambda root directory using the following command:

```bash
cmake --preset=sim-a7-100|arty-a7-100 -B <build directory>
```

For example:

```bash
cmake --preset=sim-a7-100 -B build/sim-a7-100-2
```

