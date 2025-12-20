# The Mecrisp Forth Core

- **Forth Core Software Component in the BoxLambda Directory Tree**:
 [sw/components/forth_core](https://github.com/epsilon537/boxlambda/tree/master/sw/components/forth_core)

- **Forth Core Entry Point**: [sw/components/forth_core/mecrisp-quintus-boxlambda.s](https://github.com/epsilon537/boxlambda/blob/master/sw/components/forth_core/mecrisp-quintus-boxlambda.s)

BoxLambda's Forth is based on Matthias Koch's [Mecrisp Quintus](https://mecrisp.sourceforge.net). This section discusses the Forth *Core*, i.e., the RISC-V assembly language code base that bootstraps the Forth environment.

I’ll flesh this section out over time. For now, these are my notes on the parts of the Mecrisp Forth core relevant to bringing up the codebase on BoxLambda.

## BoxLambda changes relative to the original Mecrisp Quintus Core.

This section summarizes the most important changes I made to the original
Mecrisp code base when porting it to BoxLambda.

### No Flash Memory

This is the biggest one. The original Mecrisp Forth code is written to boot from flash memory and includes mechanisms to compile Words into flash memory. These mechanisms are quite clever and make up a significant portion of the original Forth core. I don't need this feature for BoxLambda, however. Non-volatile storage on BoxLambda is primarily filesystem-based, utilizing an SD Card. BoxLambda uses flash memory for bitstreams, firmware, and key system variables, but not for storing user programs. BoxLambda's Forth does not include a flash-based dictionary.

### Two Dictionaries

The BoxLambda Forth core maintains two dictionaries: one in EMEM and one in IMEM.

Word `compiletoimem` makes the **IMEM dictionary** the primary and the EMEM
dictionary secondary. After entering this Word, any new Words created will be
compiled into IMEM.

Word `compiletoemem` makes the **EMEM dictionary** the primary and the IMEM
dictionary secondary. After entering this Word, any new Words created will be
compiled into EMEM.

After start-up, `DictionaryPointer` points to the EMEM dictionary and
`SecondDictionary` points to IMEM, i.e., initially, this system is in
*compiletoemem* state.

#### Forth Core Words

All Forth *Core* Words are entered into IMEM at initialization time.
Any Word created *after* initialization time is a *non-core* Word.

![Dictionary Search Order](assets/dictionary_search_order.png)

*Dictionary Search Order.*

A dictionary search starts from the latest non-core Word to the oldest non-core Word,
regardless of whether that non-core Word is created in EMEM or IMEM. The
search then proceeds from **oldest** Core Word (i.e., the first Word created by
the Forth core) to **newest** Core Word (the last Word created by the Forth
core).

### Booting Forth from C

The original Mecrisp Forth boots the Forth core directly from flash memory, i.e., the boot vector is part of the Forth core. BoxLambda, on the other hand, first boots up a C environment (see [here](sw_bootloader.md#boot-sequence)), then the C environment boots the Forth environment using the [Forth-C FFI](forth-c-ffi.md#the-forth-c-ffi-api) API.

From BoxLambda OS's [main.cpp](https://github.com/epsilon537/boxlambda/blob/master/sw/projects/boxlambda_os/main.cpp):

```
  forth_core_init();

  printf("Forth core init complete.\n");

  printf("Compiling Forth included_tools...\n");

  forth_load_buf((char*)included_tools, /*verbose=*/ false);

  forth_execute_word("welcome");

  forth_repl();
```

### English Translation

The original Mecrisp codebase contains many German words and comments. I took the liberty of translating these into English. In several places, comments appeared in both English and German; for the sake of brevity, I removed the redundant German comments. I hope this causes no offense.

### Conditional Compilation

The Mecrisp core is generally well written, but the nested conditional compilation used to account for the various platform variants (RISC-V 32, RISC-V 64, MIPS, compressed instruction sets, and so on) sometimes made it difficult to locate the relevant code paths. Because the BoxLambda codebase is intended solely for BoxLambda, I decided to remove the .ifdef / .else directives that don’t apply. This change significantly simplified the code.

## Understanding the Forth Core

There's a lot to be said about the Forth Core. It’s a well-written piece of code (written by Matthias Koch, that is) and well worth studying. Over time, I'll add more detail about its inner workings. Understanding the Forth Core doesn't require a lot of handholding, however. You can just dive in and enjoy the journey. When doing so, pay particular attention to the code-generating macros. Along the way, you'll realize that the Forth Core is Forth code written in assembly syntax. If you haven't looked at the code yet, that sentence probably doesn't make much sense, but you'll get it when you read the code.

The top-level source file is [mecrisp-quintus-boxlambda.s](https://github.com/epsilon537/boxlambda/blob/master/sw/components/forth_core/mecrisp-quintus-boxlambda.s). I suggest starting code reading from the beginning of that file, working your way down, recursing into each `.include` file you come across. Recursing into include files isn't something I would typically do in a C code-reading session, but for understanding the Mecrisp Forth core, it is a must.

### Key Variables

#### Forth Linker Sections and Variables

The [link map](https://github.com/epsilon537/boxlambda/blob/master/sw/projects/boxlambda_os/link.ld) defines the following sections:

- `.forth_core`: Forth core assembly code section. Part of the `.itext` section.
- `.forth_imem`: IMEM memory area reserved for Forth code.
- `.forth_emem`: EMEM memory area reserved for Forth code.

The link map also defines the following linker variables associated with those sections:

- `__forth_imem_start`: The start of IMEM memory area reserved for Forth.
- `__forth_imem_end`: The end of IMEM memory area reserved for Forth.
- `__forth_imem_size`: The amount of IMEM memory reserved for Forth.
- `__forth_emem_start`: The start of EMEM memory area reserved for Forth.
- `__forth_emem_end`: The end of EMEM memory area reserved for Forth.
- `__forth_emem_size`: The amount of EMEM memory reserved for Forth.

#### Forth Assembler Variables and Symbols

Understanding the purpose of the following variables, defined in [forth-core.s](https://github.com/epsilon537/boxlambda/blob/master/sw/components/forth_core/forth-core.s), is essential to be able to understand the Forth Core boot procedure and port it to BoxLambda:

- `DictionaryPointer`: The *primary* dictionary pointer. Dictionary search
starts here. Corresponds to Forth variable `(dp)`, taking into account that
referencing a variable in Forth puts the variable's *address* on the stack, not
its value. Word `here` puts the content of the variable on the stack. In other
words, `here`is equivalent to `' (dp) @`.
- `SecondDictionaryPointer`: The *secondary* dictionary pointer. See the [previous
section](#two-dictionaries) for a discussion of the two dictionaries.
- `ThreadEnd`: Points to the most recently defined word. Corresponds to Forth
variable `(latest)`, again taking into account that referencing a variable in
Forth puts the variable's *address* on the stack, not its value.
- `SecondThreadEnd`: Not used on BoxLambda. Set to 0.
- `VariablesPointer`: Core variables such as `base`, `state`, `>in`, hooks...
are placed and initialized at the end of the `.forth_imem` section. As core variables are placed during the
`catchmempointers.s` phase of initialization, the pointer advances downwards,
towards lower memory. When *catchmempointers* has completed, the pointer value
is stored in `VariablesPointer`. In other words, `VariablesPointer` acts like a *here*
pointer for core variables. The corresponding Word is `ramvar-here`.

The Forth Core code base relies heavily on GNU assembler preprocessing macros and symbols.
The following preprocessor symbols play an important role in the Forth core:

- `CoreDictionaryStart`: Core dictionary entry point.
- `rampointer`: Initially points to the start of the `.forth_imem` section.
Advances towards higher memory addresses as space for variables is allocated
using the `ramallot` macro.
- `CoreVariablePointer` and `DoubleCoreVariablePointer`: Initially point to
the end of IMEM. The `CoreVariable, name` macro decrements the pointer to make
space for one variable, then assigns the current pointer value to the given
symbol *name*. This allocation mechanism, executing during the assembler
preprocessing stage, matches the run-time mechanism executed by
`catchmempointer.s` to populate these variables with their initial values.

##### Inefficiencies

If you look at the definitions of core variables (e.g., `hook-emit`), you'll see
that the initial value of the variable is placed right after the code.
`Catchmempointers.s` will copy it from there to the variable location at the
end of IMEM. This is a wasteful. Each core variable takes up two IMEM
locations: one for the variable itself, one for its initial value. It results from the
boot-from-flash legacy in the original Mecrisp Forth code. I will remove this
inefficiency in a future release.

### Dictionary structure

Forth Words have the following structure in the dictionary:

```
-- Aligned on 4-byte boundary --
   4 bytes link
   4 bytes flags
   1 byte  name length
   n bytes name
-- Aligned on 4-byte boundary --
   Code
```

An empty link field or name length of zero denotes the end of the dictionary chain.

