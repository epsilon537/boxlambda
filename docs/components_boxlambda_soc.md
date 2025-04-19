---
hide:
  - toc
---

## The BoxLambda SoC Component

- **BoxLambda SoC Component in the BoxLambda Directory Tree**:
    [boxlambda/gw/components/boxlambda_soc](https://github.com/epsilon537/boxlambda/tree/master/gw/components/boxlambda_soc)

- **BoxLambda SoC Module**:
    [gw/components/boxlambda_soc/rtl/boxlambda_soc.sv](https://github.com/epsilon537/boxlambda/blob/master/gw/components/boxlambda_soc/rtl/boxlambda_soc.sv)

- **BoxLambda Top Module**:
    [gw/projects/boxlambda_top/rtl/boxlambda_top.sv](https://github.com/epsilon537/boxlambda/blob/master/gw/projects/boxlambda_top/rtl/boxlambda_top.sv)

The BoxLambda SoC module is defined as a gateware component. The [Gateware Build Structure](build_sys_gw_build_struct.md#the-gateware-build-structure) section describes the three-layer gateware build structure. The diagram below shows how the BoxLambda SoC component fits into that build structure.

![BoxLambda SoC Component Build Diagram](assets/BoxLambda_SoC_Component_Build_Diagram.png)

*Build diagram with the BoxLambda SoC component and `boxlambda_top.sv`.*

Different `gw/project/` builds reference this `boxlambda_soc` component. The project builds vary in the way they instantiate the `boxlambda_soc` module, including or excluding specific subcomponents. Most `gw/projects` reference the same [boxlambda_top.sv](https://github.com/epsilon537/boxlambda/blob/master/gw/projects/boxlambda_top/rtl/boxlambda_top.sv) module but with a different combination of *defines* in their `Bender.yml` manifest.

