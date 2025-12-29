---
layout: post
title: 'Forth and C.'
comments: true
mathjax: yes
---

In the previous post, I outlined the operating system architecture I have in mind for BoxLambda. I’ve now taken the first step toward that architecture: porting Matthias Koch’s [Mecrisp Quintus Forth](https://mecrisp.sourceforge.net) to BoxLambda and adding a Forth–C FFI, allowing C code to call Forth code and vice versa.

# Context

This is the software architecture I'm working towards:

[![BoxLambda OS Architecture.](../assets/BoxLambda_OS_Architecture_Core_and_FFI_Focus.png)](../assets/BoxLambda_OS_Architecture_Core_and_FFI_Focus.png)

*BoxLambda OS Architecture. Click to zoom.*

This post focuses on the yellow boxes.

For an overview of what BoxLambda is about and its current features, see [here](../about/).

# Terminology

I'm using the following two specific terms in this post:

- **EMEM**: External Memory (SDRAM).
- **FFI**: Foreign Function Interface, e.g. the interface used to allow C code to call Forth code.
- **IMEM**: Internal Memory.
- **Word**: When I write *Word* with a captital *W*, I'm referring to a Forth Word.
- **Non-Core Word**: A Forth Word that is not part of the Forth core.

For a complete list of terms and abbreviations used in BoxLambda, see [here](https://boxlambda.readthedocs.io/en/latest/terms-and-abbreviations/).

# BoxLambda Modifications to the Mecrisp Forth Core

BoxLambda's Forth is based on [Mecrisp Quintus version 1.1.1d](https://sourceforge.net/projects/mecrisp/files/). Mecrisp Quintus supports RISC-V32 processors, so it didn't require much effort to bring up this Forth environment on BoxLambda. Below is a summary of the changes I made relative to the original Mecrisp code.

## Modif 1: Flash Memory Dictionary Removed

This is the biggest change. The original Mecrisp Forth code boots from flash memory and compiles Words into it, a clever mechanism forming a significant part of its codebase. BoxLambda, however, relies on filesystem-based storage, using a microSD Card. Flash memory is reserved for bitstreams, firmware, and essential system variables, not user programs. Therefore, I removed flash memory support from the Forth core. Flash memory support will be added elsewhere in the system as needed, outside the Forth core.

## Modif 2: An IMEM and EMEM Dictionary

The BoxLambda Forth core maintains two dictionaries: one in EMEM and one in IMEM. The Words `compiletoimem` and `compiletoemem` are used to switch between IMEM and EMEM as the primary dictionary.

Word `compiletoimem` makes the **IMEM dictionary** the primary dictionary and the EMEM
dictionary secondary. After entering this Word, any new Words created will be
compiled into IMEM.

Conversely, Word `compiletoemem` makes the **EMEM dictionary** the primary dictionary and the IMEM
dictionary secondary. After entering this Word, any new Words created will be
compiled into EMEM.

After start-up, the `DictionaryPointer` (See [here](#forth-assembler-variables-and-symbols)) points to the EMEM dictionary and
`SecondDictionary` points to IMEM, i.e., initially, this system is in
*compiletoemem* state.

### Core Words vs. Non-Core Words

All Forth *Core* Words are defined and implemented as RISC-V32 assembly code. These Forth Core Words are assembled into IMEM.

Any Forth Word created *after* initialization time is a *Non-Core* Word.

A dictionary search starts from the latest non-core Word and proceeds to the oldest non-core Word,
regardless of whether that non-core Word is created in EMEM or IMEM. The
search then continues from **oldest** core Word (i.e., the first Word created in
the Forth core) to **newest** core Word (the last Word created in the Forth
core).

[![Dictionary Search Order](../assets/dictionary_search_order.png)](../assets/dictionary_search_order.png)

*Dictionary Search Order (click to zoom).*

## Modif 3: Booting Forth from C

The original Mecrisp Forth boots the Forth core directly from flash memory. The early boot code is part of the Mecrisp Forth core. BoxLambda, on the other hand, first boots up a C environment (see [here](https://boxlambda.readthedocs.io/en/latest/sw_bootloader/)), then the C environment boots up the Forth environment using the Forth-C FFI. The next section discusses the Forth-C FFI in more detail.

From BoxLambda OS's [main.cpp](https://github.com/epsilon537/boxlambda/blob/master/sw/projects/boxlambda_os/main.cpp):

```
  forth_core_init();
  printf("Forth core init complete.\n");
  printf("Compiling Forth init.fs...\n");
  /* init_fs is a long text string containing auxilliary
   * Forth Words, such as the disassembler.*/
  forth_load_buf((char*)init_fs, /*verbose=*/ false);
  forth_execute_word("welcome");
  forth_repl();
```

## Modif 4: English Translation

The original Mecrisp codebase contains many German words and comments. I translated these into English. In other places, comments appeared both in English and in German. For the sake of brevity, I removed the redundant German comments (I hope this causes no offense).

## Modif 5: Conditional Compilation

The Mecrisp core is well-written, but the nested conditional compilation used to account for the various platform variants sometimes made it difficult to locate the relevant code paths. Because the BoxLambda codebase is intended solely for BoxLambda, I decided to remove the `.ifdef` / `.else` directives that don’t apply. This change significantly simplified the code.

# Understanding the Forth Core

Matthias Koch created a beautiful piece of code. The Mecrisp Forth core is well
worth studying. Understanding the Forth core doesn't require a lot of
handholding. You can just dive in and enjoy the journey. When doing
so, pay particular attention to the code-generating macros. Along the way,
you'll realize that the Forth Core is Forth code written in assembly syntax. If
you haven't looked at the code yet, that sentence might not make much sense,
but you'll get it when you see it. Here is a sample:

```
# -----------------------------------------------------------------------------
  Definition Flag_visible, "c,"
ckomma: # Write 8 bits in Dictionary
# -----------------------------------------------------------------------------
  push x1 # Push return address to Return Stack

  call here

  pushdaconst 1 # Push value 1 onto the data stack.
  call allot    # Allocate 1 byte

  popda x15 # Pop from data stack into x15.
  sb x8, 0(x15) # Write the byte. x8 is Top-of-Stack (TOS)
  drop

  pop x1
  ret
```

The top-level source file is [mecrisp-quintus-boxlambda.s](https://github.com/epsilon537/boxlambda/blob/master/sw/components/forth_core/mecrisp-quintus-boxlambda.s). I suggest starting code reading from the beginning of that file, working your way down, and recursing into each `.include` file you come across. Recursing into include files isn't something I would typically do in a C code-reading session, but for understanding the Mecrisp Forth core, it is a must.

[![Forth Core Org](../assets/forth-core_org.png)](../assets/forth-core_org.png)

*Forth Core Organization.*

## Key Variables

Understanding the purpose of the following variables, defined in [forth-core.s](https://github.com/epsilon537/boxlambda/blob/master/sw/components/forth_core/forth-core.s), is essential to be able to understand the Forth Core:

- `DictionaryPointer`: The *primary* dictionary pointer. Dictionary search
starts here. `DictionaryPointer` corresponds to Forth variable `(dp)`, taking into account that
referencing a variable in Forth puts the variable's *address* on the stack, not
its value. The Word `here` puts the content of the variable on the stack. In other
words, `here`is equivalent to `' (dp) @`.
- `SecondDictionaryPointer`: The *secondary* dictionary pointer.
- `ThreadEnd`: Points to the most recently defined word. `ThreadEnd` corresponds to Forth
variable `(latest)`, again taking into account that referencing a variable in
Forth puts the variable's *address* on the stack, not its value.
- `SecondThreadEnd`: Not used on BoxLambda. Set to 0.
- `VariablesPointer`: Core variables such as `base`, `state`, `>in`, hooks...
are placed and initialized at the end of the `.forth_imem` linker section. As core variables are placed during the
`catchmempointers.s` phase of initialization, the pointer advances downwards,
towards lower memory. When `catchmempointers` has completed, the pointer value
is stored in `VariablesPointer`. In other words, `VariablesPointer` acts like a *here*
pointer for core variables. The corresponding Word is `ramvar-here`.

The Forth Core code base relies heavily on GNU assembler preprocessing macros and symbols.
The following preprocessor symbols play an important role in the Forth core:

- `CoreDictionaryStart`: Core dictionary entry point.
- `rampointer`: Initially points to the start of the `.forth_imem` linker section.
Advances towards higher memory addresses as space for variables is allocated
using the `ramallot` macro.
- `CoreVariablePointer` and `DoubleCoreVariablePointer`: Initially point to
the end of IMEM. The `CoreVariable, name` macro decrements the pointer to make
space for one variable, then assigns the current pointer value to the given
symbol *name*. This allocation mechanism, executing during the assembler
preprocessing stage, matches the run-time mechanism executed by
`catchmempointer.s` to populate these variables with their initial values.

### Inefficiencies

If you look at the definitions of core variables (for example, [hook-emit](https://github.com/epsilon537/boxlambda/blob/master/sw/components/forth_core/terminalhooks.s)), you’ll see that each variable’s initial value is placed immediately after its code. [catchmempointers.s](https://github.com/epsilon537/boxlambda/blob/master/sw/components/forth_core/catchmempointers.s) then copies this value to the variable’s location at the end of IMEM. This is wasteful: each core variable occupies two IMEM locations—one for the variable itself and one for its initial value. This behavior is a legacy of the boot-from-flash design. I plan to fix this in a future release.

# The Forth-C Foreign Function Interface (FFI)

API:

- [sw/components/forth_core/forth.h](https://github.com/epsilon537/boxlambda/blob/master/sw/components/forth_core/forth.h)

Implementation:

- [sw/components/forth_core/forth.cpp](https://github.com/epsilon537/boxlambda/blob/master/sw/components/forth_core/forth.cpp)
- The `c-fun` Word in [sw/components/forth_core/init.fs](https://github.com/epsilon537/boxlambda/blob/master/sw/components/forth_core/init.fs)
- Forth-to-C: [sw/components/forth_core/c-ffi.s](https://github.com/epsilon537/boxlambda/blob/master/sw/components/forth_core/c-ffi.s)
- C-to-Forth: [sw/components/forth_core/mecrisp-quintus-boxlambda.s](https://github.com/epsilon537/boxlambda/blob/master/sw/components/forth_core/mecrisp-quintus-boxlambda.s)

The BoxLambda C-Forth FFI is loosely based on Peter Schmid's work for the [Mecrisp Cube project](https://github.com/spyren/Mecrisp-Cube/tree/master).

C uses a single stack to manage the call stack, stack frames (local variables), and—when many parameters are involved—parameter passing. Forth, by contrast, uses two stacks: a *Return Stack* and a *Data Stack*. The Return Stack is used to track the call stack and stack frames, while the Data Stack is used for parameter passing.

On BoxLambda, the C stack acts as a return stack when in Forth space. It's a natural
fit. It doesn't require any additional software constructs. The C compiler
manages the C stack, while on the Forth side, the Mecrisp Quintus Forth Core is already using the
RISC-V stack pointer register (x2) as the return stack pointer.

The Data Stack does require a software construct. The [forth_core_init_](https://github.com/epsilon537/boxlambda/blob/master/sw/components/forth_core/mecrisp-quintus-boxlambda.s) assembly function initializes a global `datastack` object with the following layout in C (see [forth.h](https://github.com/epsilon537/boxlambda/blob/master/sw/components/forth_core/forth.h)):

```
typedef struct {
  uint32_t tos;
  uint32_t *psp;
} Forth_Datastack;

extern Forth_Datastack datastack;
```

The `tos` field is the *Top Of (data) Stack*. The `psp` field is the *Parameter Stack Pointer*. The Parameter Stack is an old-school term for Data Stack.

This `datastack` object acts as the mailbox between C and Forth.

In C space, the data stack is manipulated using the following two accessors:

```
// Push a value onto the data stack
void forth_pushda(uint32_t val);

// Pop a value from the data stack
uint32_t forth_popda();
```

## C Calling Forth

When C calls Forth, the calling function puts input parameters on the data stack using `forth_pushda()` before invoking the Forth Word. Afterwards (i.e., when the Forth Word has executed and returned to C), it picks up any return values using `forth_popda()`.

### Example 1

Here's an example of C calling Forth:

```
  uint32_t first_arg = 42;
  uint32_t second_arg = 43;

  forth_pushda(second_arg);
  forth_pushda(first_arg);

  forth_execute_word("foo");

  uint32_t res1 = forth_popda();
  uint32_t res2 = forth_popda();

  printf("Foo returned %d and %d.\n", res1, res2);
```

In this example, `foo` is a Word that takes two arguments and returns two values. For example:

```
: foo ." In foo..." 2dup . . cr /mod ;
```

[![C Calls Forth.](../assets/c-calls-forth.png)](../assets/c-calls-forth.png)

*C Calling Forth (click to zoom).*

### Example 2

If C needs to call Forth Word `foo` many times, it's better to store foo's execution token (*xt*) so we don't have to do a dictionary search each time to find the definition:

```
  uint32_t foo_xt = forth_find_word("foo");

  uint32_t first_arg = 42;
  uint32_t second_arg = 43;
  uint32_t res1, res2;

  for (int ii=0; ii<100; ii++) {
      forth_pushda(second_arg);
      forth_pushda(first_arg);

      forth_execute_xt(foo_xt);

      res1 = forth_popda();
      res2 = forth_popda();
      printf("Foo returned %d and %d.\n", res1, res2);
  }
```

## Forth Calling C

When Forth calls C, it updates the `datastack` object with its current *TOS* and *PSP* values. It then invokes one of the registered C functions. The registered C function retrieves input parameters from the stack using `forth_popda()` and pushes output parameters/return values on the stack using `forth_pushda()`.

C functions are registered with Forth using the following macro from [forth.h](https://github.com/epsilon537/boxlambda/blob/master/sw/components/forth_core/forth.h):

```
// Register a C function with signature: void fun(void). Fun uses the datastack object for parameter passing.
#define forth_register_cfun(fun, wordname) \
           forth_pushda((uint32_t)fun), forth_eval("c-fun " wordname)
```

Note the expected ```void fun(void)``` signature. Forth registered C functions don't have any C-style input parameters or return values. The `datastack` object is used exclusively for parameter passing.

### Example

Let's say we have a function `cbar()` which takes two input parameters off the data stack, prints them, and then pushes values 77 and 88 on the stack as output arguments:

```
void cbar() {
  uint32_t first_arg = forth_popda();
  uint32_t second_arg = forth_popda();

  printf("test_c_fun called with args %d and %d.\n", first_arg, second_arg);

  printf("Returning values 77 88 to Forth.\n");

  forth_pushda(77);
  forth_pushda(88);
}
```

Here's an example of Forth calling this C function with input values 11 and 22:

Preparation in C space, at initialization time:

```
  forth_core_init();
  forth_register_cfun(cbar, "cbar");
```

Now `cbar` can be invoked from Forth like a regular Forth Word:

```
cr 11 22 cbar . . cr
test_c_fun called with args 22 and 11.
Returning values 77 88 to Forth.
88 77
ok
```

(The `. . cr` after calling `cbar` pops the output arguments off the stack, prints them, and then prints a carriage return.)

[![Forth Calls C.](../assets/forth-calls-c.png)](../assets/forth-calls-c.png)

*Forth Calling C (click to zoom).*

## Loading init.fs

The file [init.fs](https://github.com/epsilon537/boxlambda/blob/master/sw/components/forth_core/init.fs) contains auxilliary Forth Words, such as the disassembler. At the moment, BoxLambda OS [initialization code](https://github.com/epsilon537/boxlambda/blob/master/sw/projects/boxlambda_os/main.cpp) is loading init.fs as a single, large text string, using the `forth_load_buf()` function:

```
  forth_load_buf((char*)init_fs, /*verbose=*/ false);
```

This is only placeholder code, however. Once filesystem support is in place, init.fs will be loaded directly from the filesystem.

## Register Usage

When C code calls a Forth Word, or a Forth Word calls C, we have to consider
both the [RISC-V C
ABI](https://riscv.org/wp-content/uploads/2024/12/riscv-calling.pdf) and the
Mecrisp Quintus Forth register usage convention.

### Register Usage in case of C Calling Forth

To maintain the C environment when C calls a Forth Word, the Forth code has to execute as if it were a C function.

The table below summarizes the actions to be taken per register when C calls Forth:

| Register to preserve | C ABI Name | C ABI Description | Mecrisp Forth Description | Required action when C calls Forth |
|----------|------------|-------------------|---------------------------|-----------------------------------|
| x0     | zero  | Hard-wired zero | Hard-wired zero | None. |
| x1     | ra    | Return address  | Return address | Save on RS before calling Forth Word, restore afterwards. |
| x2     | sp    | Stack pointer, must be 16-byte aligned | Return Stack pointer (RS) | None. Forth maintains the C stack as RS |
| x3     | gp    | Global pointer  | Loop index | Restore gp when returning back to C. |
| x4     | tp    | Thread pointer  | Loop limit | Restore tp when returning back to C. |
| x5     | t0    | Temporary, not preserved across calls | Scratch register, must be saved before use | None. |
| x6     | t1    | Temporary, not preserved across calls | Scratch register, must be saved before use | None. |
| x7     | t2    | Temporary, not preserved across calls | Scratch register, must be saved before use | None. |
| x8     | s0/fp | Saved register/frame pointer, preserved across calls/must be saved before use | Top of data stack (TOS) | x8=datastack.tos upon entry. Datastack.tos=x8 when returning to C. |
| x9     | s1    | Saved register, preserved across calls/must be saved before use | Data stack pointer (PSP) | x9=datastack.psp upon entry. Datastack.psp=x9 when returning to C. |
| x10    | a0    | Function argument/return value, not preserved across calls/no need save before use | Scratch register, must be saved before use | None. |
| x11    | a1    | Function argument/return value, not preserved across calls/no need save before use | Scratch register, must be saved before use | None. |
| x12    | a2    | Function argument, not preserved across calls/no need save before use | Scratch register, must be saved before use | None. |
| x13    | a3    | Function argument, not preserved across calls/no need save before use | Scratch register, must be saved before use | None. |
| x14    | a4    | Function argument, not preserved across calls/no need save before use | Free scratch register, not preserved across calls | None. |
| x15    | a5    | Function argument, not preserved across calls/no need save before use | Free scratch register, not preserved across calls | None. |
| x16-31 | --    | -- | Not used in vanilla forth core | None. |

Because BoxLambda is a single-threaded platform, I can save the global pointer (x3/gp)
and thread pointer (x4/tp) once, during Forth core initialization. I store them in
global variables, so they can easily be retrieved when needed.

I also save the C stack pointer (x2/sp) into a global variable upon entry
into Forth so I can restore the stack to this point if the Forth `reset` Word is
invoked.

See `forth_core_init_` and `forth_core_fun_` in [mecrisp-quintus-boxlambda.s](https://github.com/epsilon537/boxlambda/blob/master/sw/components/forth_core/mecrisp-quintus-boxlambda.s).

### Register Usage in case of Forth Calling C

To maintain the Forth environment when Forth calls a C function, the C code has to execute as if it were a Forth Word.

The table below summarizes the actions to be taken per register.

| Register to preserve | C ABI Name | C ABI Description | Mecrisp Forth Description | Required action when Forth calls C |
|----------|------------|-------------------|---------------------------|-----------------------------------|
| x0     | zero  | Hard-wired zero | Hard-wired zero | None. |
| x1     | ra    | Return address  | Return address | Save on RS before calling C, restore afterwards. |
| x2     | sp    | Stack pointer, must be 16-byte aligned | Return Stack pointer (RS) | Save on RS, 16-byte align before calling C, restore afterwards. |
| x3     | gp    | Global pointer  | Loop index | Save on RS and switch to gp before calling C, restore afterwards. |
| x4     | tp    | Thread pointer  | Loop limit | Save on RS and switch to tp before calling C, restore afterwards. |
| x5     | t0    | Temporary, not preserved across calls | Scratch register, must be saved before use | Save on RS before Calling C, restore afterwards. |
| x6     | t1    | Temporary, not preserved across calls | Scratch register, must be saved before use | Save on RS before Calling C, restore afterwards. |
| x7     | t2    | Temporary, not preserved across calls | Scratch register, must be saved before use | Save on RS before Calling C, restore afterwards. |
| x8     | s0/fp | Saved register/frame pointer, preserved across calls/must be saved before use | Top of data stack (TOS) | datastack.tos=x8 upon entry. X8=datastack.tos when returning to Forth. |
| x9     | s1    | Saved register, preserved across calls/must be saved before use | Data stack pointer (PSP) | Datastack.psp=x9 upon entry. X9=datastack.psp when returning to Forth. |
| x10    | a0    | Function argument/return value, not preserved across calls/no need save before use | Scratch register, must be saved before use | Save on RS before Calling C, restore afterwards. |
| x11    | a1    | Function argument/return value, not preserved across calls/no need save before use | Scratch register, must be saved before use | Save on RS before Calling C, restore afterwards. |
| x12    | a2    | Function argument, not preserved across calls/no need save before use | Scratch register, must be saved before use | Save on RS before Calling C, restore afterwards. |
| x13    | a3    | Function argument, not preserved across calls/no need save before use | Scratch register, must be saved before use | Save on RS before Calling C, restore afterwards. |
| x14    | a4    | Function argument, not preserved across calls/no need save before use | Free scratch register, not preserved across calls | None. |
| x15    | a5    | Function argument, not preserved across calls/no need save before use | Free scratch register, not preserved across calls | None. |
| x16-31 | --    | -- | Not used in vanilla forth core | None. |

See `call-c` in [c-ffi.s](https://github.com/epsilon537/boxlambda/blob/master/sw/components/forth_core/c-ffi.s) along with Word `c-fun` in [init.fs](https://github.com/epsilon537/boxlambda/blob/master/sw/components/forth_core/init.fs).

# The Forth Word List

The current Forth Word list can be found [here](https://boxlambda.readthedocs.io/en/latest/forth-glossary/).

# Try It Out

For instructions to build and flash the BoxLambda Gateware, Bootloader and OS, see [here](installation.md#installing-the-boxlambda-base-bitstream-bootloader-and-os).

Once booted up, you're dropped in the Forth REPL and can start entering Forth instructions. Here's a quick example hooking up a timer ISR and setting a one-shot timer:

```

0 1 nvariable timer-irq-received
: mtime-irq-handle
  -1 s>d mtimecmp64! \ Disable timer
  ." MTIMER IRQ received." cr
  true timer-irq-received ! ;

: wait-for-timer-irq
  ." Waiting for MTIME IRQ..." cr begin timer-irq-received @ until ;

\ Initially disable everything.
dint
0 mie!
-1 s>d mtimecmp64! \ Disable timer

\ Hook up timer ISR.
' mtime-irq-handle irq-timer !

\ Enable IRQ and set a timer 6s from now.
1 irq-id-timer lshift mie! eint
50000000 6 * set-raw-time-cmp

\ Wait for it...
wait-for-timer-irq

\ Disable interrupts again.
dint
```

You can take a look at the generated code using the disassembler:

```
see mtime-irq-handle
2001923C: FFC48493  addi   x9, x9, -4
20019240: 0084A023  sw     x8, 0 (x9)
20019244: FFF04413  xori   x8, zero, -1
20019248: FFC48493  addi   x9, x9, -4
2001924C: 0084A023  sw     x8, 0 (x9)
20019250: FFF04413  xori   x8, zero, -1
20019254: FFC10113  addi   x2, x2, -4
20019258: 00112023  sw     x1, 0 (x2)
2001925C: 000017B7  lui    x15, 00001000
20019260: A48780E7  jalr   x1, -5B8 (x15)  --> mtimecmp64!
20019264: 000017B7  lui    x15, 00001000
20019268: 558780E7  jalr   x1, 558 (x15)  --> (.")  -->  ." MTIMER IRQ received."
2001926C: 49544D14
20019270: 2052454D
20019274: 20515249
20019278: 65636572
2001927C: 64657669
20019280: 0000002E
20019284: 000017B7  lui    x15, 00001000
20019288: 394780E7  jalr   x1, 394 (x15)  --> cr
2001928C: FFF04793  xori   x15, zero, -1
20019290: 20019737  lui    x14, 20019000
20019294: 20F72E23  sw     x15, 21C (x14)
20019298: 00012083  lw     x1, 0 (x2)
2001929C: 00410113  addi   x2, x2, 4
200192A0: 00008067  jalr   zero, 0 (x1)
 ok.
```

## The Forth Core Test Suite

To build and run the Forth Core test suite, see [here](https://boxlambda.readthedocs.io/en/latest/sw-test-build-forth-core/).

# Conclusion

I can now write and compile code directly on BoxLambda, without relying on a cross-compiler. That's a significant step forward, made possible by Mecrisp Quintus Forth. There’s much more to say about Mecrisp Forth — I haven’t even touched on its centerpiece, the compiler itself. I plan to devote future posts to deep dives into specific aspects of the Forth core. For now, though, I still have most of an operating system left to build.

Next, I’ll be integrating the FatFS filesystem into the Forth environment.

# References

- [Mecrisp Forth](https://mecrisp.sourceforge.net).
- [Mecrisp Stellaris Unofficial Documentation](https://mecrisp-stellaris-folkdoc.sourceforge.io/index.html#index).
- [Mecrisp Cube](https://github.com/spyren/Mecrisp-Cube/tree/master).
- [BoxLambda Documentation](https://boxlambda.readthedocs.io/en/latest/).


