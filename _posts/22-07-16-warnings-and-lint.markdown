---
layout: post
title: 'Warnings and Verilator Lint.'
comments: true
---

Recap
-----
We currently have a simple *Hello World* test project for an Arty-A7-35T, consisting of an Ibex RISCV core, a Wishbone shared bus, some internal memory, a timer, GPIO, and UART core. We can build a simple *Hello World* test program for the processor and include that into the FPGA build. Software compilation and FPGA synthesis and implementation are managed by a Makefile and Bender based build system.

The *Hello World* test project currently builds and runs just fine. However, from the number of warnings that Vivado spits outs during synthesis, you would almost be surprised it works at all. Since my [previous post](https://epsilon537.github.io/boxlambda/make-and-bender/), I've been sorting through those warnings. I also added linting.

Vivado Warnings
---------------
If like me, you have a software background, you'll probably see warnings as errors. They're often benign but, ideally, they should be fixed.

Vivado synthesis doesn't seem to work like that. Vivado generates warnings for code that, to me at least, looks perfectly alright. For example: 

You attach a simple slave to a shared bus. The slave doesn't require all input signals from the bus (e.g. a subset of the address lines). The slave also drives some of the optional output signals to a constant zero (e.g. an error signal). 

When synthesizing this slave module, Vivado will generate a warning for each unconnected input signal and for each output signal that's driven by a constant. In other words: in Vivado, **Warnings are not Errors**. Warnings need to be reviewed, but they don't necessarily need to be fixed.

Btw, I'm just referring to regular Vivado warnings here. Vivado may also generate *Critical Warnings*. Critical Warnings indicate significant issues that need to be looked at and fixed.

Synthesizing a component separately also generates a lot of additional warnings, compared to synthesizing that same component embedded in a project build, with all the inputs, outputs, and clocks hooked up. Many of those warnings can be avoided by adding constraints specifically for the standalone synthesis of that component, but I don't think it's worth the effort. I decided to focus instead on reviewing and fixing as many warnings as possible in project builds. Right now, that's just the *Hello World* build.

There's also the matter of warnings deep inside third-party code. Warnings near a component's *surface* you have to be careful with, as those can point to integration issues. Several layers deep, however, you're looking at third-party code internals that is presumably being actively maintained by someone else. I take a look when I see such a warning, but I will think twice before making changes. On the other hand, abandoned third-party code, such as ibex_wb, I will treat as my own. 

To summarize, here's how I'm handling Vivado warnings:

- **Critical Warnings are Errors**. They need to be looked at and fixed.
- **(Regular) Warnings are not Errors**. They need to be looked at, but not necessarily fixed.
- **Focus on project build warnings**. Never mind the standalone component synthesis warnings.
- **Think twice before fixing warnings inside actively maintained third-party code.**

With that pragmatic mindset adopted, I was able to make progress. I fixed a bunch of warnings, but not all, for the reasons stated above.

Lint Checking
-------------
Because Vivado synthesis spits out such confusing warnings, I wanted a second opinion. I decided to add **Verilator lint** checking to the build system. Verilator lint performs static code analysis and will find coding issues that Vivado synthesis often does not. Moreover, it does this very quickly. Without linting, finding and fixing coding errors is a slow process: 

1. Make some code changes.
2. Kick-off synthesis.
3. Wait **20 minutes or more** for the synthesis to complete.
4. Get a bunch of warnings and/or errors.
5. Repeat.

With lint on the other hand:

1. Make some code changes.
2. Kick-off lint checking.
3. Wait **10 seconds**.
4. Get a bunch of warnings and/or errors.
5. Repeat.

When your design lints cleanly, you still need to synthesize it obviously, but at that point, it should take far fewer synthesis cycles compared to doing the same thing without linting.

Verilator Lint Waivers
======================
It's common to insert lint waivers into code, telling the lint checker to not issue a particular warning when checking a particular piece of code:

```
   // There are missing pins here, but the arty-a7 example in the ibex repository
   // is instantiated the same way, so I'm sticking to it.
   // verilator lint_off PINMISSING
   ibex_top #(
     ...
  );

   // verilator lint_on PINMISSING
```

Inserting lint waivers into your own source code is fine, but it's annoying to insert waivers into third-party code. You end up with a bunch of little deviations from the vanilla code base. Those deviations turn into a bunch of little merge conflicts down the road when you *git pull* the latest-and-greatest from the third-party repository. 

You can avoid that issue by putting lint waivers in separate *.vlt* files instead of inserting them directly into source code. In *.vlt* files, you can specify to which file, and code block within a file, to apply the waiver. For instance, my *.vlt* file for the ibex component looks like this:

```
`verilator_config
lint_off -rule UNUSED -file "*/sub/ibex/build/lowrisc_ibex_top_artya7_0.1/src/lowrisc_ibex_ibex_core_0.1/rtl/ibex_compressed_decoder.sv"
lint_off -rule UNUSED -file "*/sub/ibex/build/lowrisc_ibex_top_artya7_0.1/src/lowrisc_ibex_ibex_pkg_0.1/rtl/ibex_pkg.sv"
lint_off -rule UNUSED -file "*/sub/ibex/build/lowrisc_ibex_top_artya7_0.1/src/lowrisc_prim_cipher_pkg_0.1/rtl/prim_cipher_pkg.sv"
lint_off -rule UNUSED -file "*/sub/ibex/build/lowrisc_ibex_top_artya7_0.1/src/lowrisc_prim_generic_clock_gating_0/rtl/prim_generic_clock_gating.sv"
lint_off -rule UNUSED -file "*/sub/ibex/build/lowrisc_ibex_top_artya7_0.1/src/lowrisc_prim_ram_1p_pkg_0/rtl/prim_ram_1p_pkg.sv"
lint_off -rule UNUSED -file "*/sub/ibex/build/lowrisc_ibex_top_artya7_0.1/src/lowrisc_prim_ram_2p_pkg_0/rtl/prim_ram_2p_pkg.sv"
lint_off -rule UNUSED -file "*/sub/ibex/build/lowrisc_ibex_top_artya7_0.1/src/lowrisc_prim_secded_0.1/rtl/prim_secded_pkg.sv"
```

I have checked this in as *lint.vlt* into the *components/ibex/* directory. No changes are required in the *sub/ibex/* repository.

You can find more info on *.vlt* configuration files here:

[https://verilator.org/guide/latest/exe_verilator.html#configuration-files](https://verilator.org/guide/latest/exe_verilator.html#configuration-files).

New Build Targets
=================
I added new targets to the *Bender.yml* files to accommodate lint checking. We currently have the following Bender targets:

- ***module_name***: set when building a component separately (i.e. running **make synth** in a component directory). For example:

```
  - target: ibex_wb_core
    files:
      - rtl/ibex_wb_core_wrapper.sv    
```

- **vivado**: set when synthesizing using Vivado.
- **verilator**: set when linting using Verilator.
- **memory**: set when retrieving memory files for this component or project.
- **constraints**: set when retrieving *.xdc* constraints files for this component or project.
- **vlt**: set when retrieving *.vlt* verilator configuration files.

I also added new Makefile targets:

- **make lint** in a component or project directory runs lint checking on that component/project and all of its dependencies.
- **make lint** in the root directory will recursively run *make lint* in each component and project directory. I use it as a sanity check across the entire repository.
- **make synth** in the root directory will recursively run *make synth* in each component and project directory. I use it as a sanity check across the entire repository.

**make lint** currently completes without errors or warnings on all component and project directories. The goal is to keep it that way.

Try It Out
----------

To try out the latest code:

0. Install the [prerequisites](https://boxlambda.readthedocs.io/en/latest/prerequisites/). 
1. **git clone https://github.com/epsilon537/boxlambda/**,
2. **cd boxlambda**
3. Switch to the *warnings_and_lint* tag: **git checkout warnings_and_lint**.
4. Get the submodules: **git submodule update --init --recursive**.
5. Run a lint check across all components and projects: **make lint** (from the repository root directory)
6. And/Or build the project:
   1. **cd projects/hello_world**
   2. **make impl**
7. Start Vivado and download the generated bitstream to your Arty A7-35T: *projects/hello_world/generated/project.runs/impl_1/ibex_soc.bit*

Interesting Links
-----------------
[FPGA Prototyping by SystemVerilog Examples: Xilinx MicroBlaze MCS SoC Edition](https://www.amazon.com/FPGA-Prototyping-SystemVerilog-Examples-MicroBlaze/dp/1119282667): A link to a book, haha! Unfortunately, not everything is freely and legally available online yet. This is the first book I read about FPGA development. It's not perfect, but it is pretty good. The book is easy to follow and engaging because it's hands-on: By the time you complete the last chapter, you'll have a working VGA graphics core with a frame buffer, text overlay, mouse pointer, and sprites. You'll also have a sound core, PS/2 mouse and keyboard, a UART, and SD storage.
