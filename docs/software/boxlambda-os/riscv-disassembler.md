# Mecrisp RISC-V 32 IM Disassembler

[disasm.fs](../../../fs/forth/boxkern-includes/disasm.fs)
[dump.fs](../../../fs/forth/boxkern-includes/dump.fs)

`disasm-$`

- Variable holding current position for disassembling.

`disasm-step ( -- )`

- Disassemble instruction at `disasm-$` and advance `disasm-$`.

`list ( -- )`

- A quick list of Words.

`see ( "Word" -- )`

- Disassemble the definition of Word from beginning to first `ret`.

`dump ( addr len -- )`

- Print the contents of a memory region.

