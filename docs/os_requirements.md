# OS Requirements

The [original requirements](requirements.md) relevant for the software side of BoxLambda
are:

- **Simplicity**: It must be easy to jump in and do something: create, hack,
tinker.
- **Grokability**: It must be doable for a single person to develop a good
understanding of the entire system, software and hardware.
- **Deterministic Behavior**: By design, it must be clear how long an operation
  an operation is going to take, be it one instruction or a complete function.
- **Single User/Single Tasking OS** booting to a console shell.

(There are some additional, practical requirements for peripherals, sound, and
graphics. Those are less relevant for the current discussion.)

## Additional Requirements

I wrote down the original requirements three years ago. Meanwhile, I've had
time to think about them and my expectations for the operating system. I would
like to introduce the following additional requirements:

- **Self-Hosting**: The system must be self-contained and self-hosted, i.e. not
depend on a host PC for software development.
- **System Programming**: The programmer must have full access to the system,
i.e. not be restricted to a sandbox/VM environment.
- **REPL**: The programming environment must support interactive programming
using a REPL.
- **Automated Testing**: All features must support automatic testing.

Elaborating on the above:

- The OS must support assembly-language level programming. Think of programming
an Interrupt Service Routine or bit-banging a timing-critical bus, for example.
- I don't want to do all system-level programming in assembly language. I'm
going to need a **Compiled Language**. The compiler must be self-hosted.
- The OS must include low-level tools such as a disassembler, memory editor, and
debugging facilities.
- The OS must support a text editor.

Also worth noting:

- The *Automatic Testing* requirement may result in gateware changes supporting
testability, e.g. a VGA capture mechanism.
- A purely interpreted software environment does not meet the requirements (it
is not suitable for *System Programming* and does not easily provide
*Deterministic Behavior*).
- Even though the system must not *depend* on a host PC, a USB connection to a PC
will still be useful for:
    - File transfers to or from a PC.
    - JTAG debugging of system software issues.
    - Flashing new firmware and bitstream images.
- I'm going to use open-source third-party components wherever I see fit, just
like I did on the Gateware side of the project. In practice, this means that
parts of the OS will consist of cross-compiled C/C++ code. An *FFI* (*Foreign
Function Interface*) between the cross-compiled code and the self-hosted code
will be needed.
<br/><br/>

## Forth

Forth enthusiasts already know: These requirements have *Forth* written all over
them.

Forth is a unique, minimalistic, extremely powerful programming language,
perfect for running on constrained systems. It features a REPL, an interpreter
doing double duty as a compiler (a *competer*? *interpiler*?), an assembler,
editor, and metaprogramming capabilities you wouldn't believe.

To get a sense of what a unique language Forth is, check out this article. It's
long but quite entertaining:

[![the programming language that writes itself.](assets/forth_article.png)](https://ratfactor.com/forth/the_programming_language_that_writes_itself.html)

Forth just begs to be explored on an experimental homebrew computer such as
BoxLambda. Learning Forth is *not* a walk in the park. It requires a different
way of thinking. For a beginner, an average line of low-level Forth is a
puzzle. Here's an example:

```
: IF IMMEDIATE ' 0BRANCH , HERE @ 0 , ;

: THEN IMMEDIATE DUP HERE @ SWAP - SWAP ! ;
```

Two lines of code, defining the equivalent of the ```if (...) {...}``` control
structure in C. How many lines of code would a C compiler require to implement
```if (...) {...}```? Needless to say, there's a lot to unpack in those two lines
of Forth code. But I enjoy the challenge! It feels a bit like diving into assembly
code on the Commodore 64 for the first time. <br/><br/>

