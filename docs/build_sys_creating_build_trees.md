Creating additional build trees
-------------------------------
You can easily create additional build trees from the BoxLambda root directory with the following command:

```
cmake --preset=sim-a7-35|sim-a7-100|arty-a7-35|arty-a7-100 -B <build directory>
```

For example:

```
cmake --preset=sim-a7-100 -B build/sim-a7-100-2
```
