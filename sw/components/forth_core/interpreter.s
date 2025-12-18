#    BoxLambda port of Mecrisp Quintus Forth by Ruben Lysens/Epsilon537.
#    Original header below:
#
#    Mecrisp-Quintus - A native code Forth implementation for RISC-V
#    Copyright (C) 2018  Matthias Koch
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

# Interpreter and optimisations

# -----------------------------------------------------------------------------
  Definition Flag_visible, "evaluate" # ( -- )
# -----------------------------------------------------------------------------
evaluate:
  push x1
  call source             # Save current source

  # 2>r
  popdadouble x15, x14
  pushdouble x14, x15

  laf x14, Bufferlevel  # Save >in and set to zero
  lc x15, 0(x14)
  push x15
  li x15, 0
  sc x15, 0(x14)

  call setsource          # Set new source
  call interpret          # Interpret

  laf x14, Bufferlevel  # Restore >in
  pop x15
  sc x15, 0(x14)

  # 2r>
  popdouble x15, x14
  pushdadouble x14, x15

  pop x1
  j setsource          # Restore old source

#------------------------------------------------------------------------------
  Definition Flag_visible|Flag_variable, "hook-interpret" # ( -- addr )
  CoreVariable hook_interpret
#------------------------------------------------------------------------------
  pushdaaddrf hook_interpret
  ret
  .varinit interpret_vanilla

# -----------------------------------------------------------------------------
  Definition Flag_visible, "interpret" # ( -- )
interpret:
# -----------------------------------------------------------------------------

  laf x15, hook_interpret
  lc x15, 0(x15)

  jalr zero, x15, 0

# -----------------------------------------------------------------------------
  Definition Flag_visible, "(interpret)" # ( -- )
interpret_vanilla:
# -----------------------------------------------------------------------------
  push x1

1: # Stay in loop as long as token can fetch something from input buffer.

  # Check pointer for datastack.

  laf x10, __datastack
  bgeu x10, x9, 2f
    writeln "Stack underflow"
    j quit

2:laf x10, __datastack_end
  bltu x10, x9, 3f
    writeln "Stack overflow"
    j quit

3: # Stacks are fine.

# -----------------------------------------------------------------------------

  # Set Constant-Folding-Pointer if not set already
  laf x14, constantfoldingpointer
  lc x7, 0(x14)
  bne x7, zero, 3f
    mv x7, x9
    sc x7, 0(x14)
3:

# -----------------------------------------------------------------------------
  call token
  # ( Address Length )

  # Prüfe, ob der String leer ist  Check if token is empty - that designates an empty input buffer.
  bne x8, zero, 2f
    ddrop
    pop x1
    ret
2:

# -----------------------------------------------------------------------------
  # We have a string to interpret.
  # ( Address Length )

  ddup
  # ( Address Length Address Length)
  call find # Attempt to find token in dictionary.
  # ( Token-Addr Token-Length Addr Flags )

  popdadouble x11, x12
  #popda x11 # Flags
  #popda x12 # Entry Address

  # ( Token-Addr Token-Length )

  # Registers:
  #  x11: Flags
  #  x12: Code entry point
  #  x7: Constant folding pointer

  bne x12, zero, 4f
    # Entry-Address is zero if not found ! Note that Flags have very special meanings in Mecrisp !

    lc x10, 0(x9)
    mv x11, x8

    call number

  # Number gives back ( 0 ) or ( x 1 ).
  # Zero means: Not recognized.
  # Note that literals actually are not written/compiled here.
  # They are simply placed on stack and constant folding takes care of them later.

    popda x12   # Flag von Number holen
    bne x12, zero, 1b # Did number recognize the string ?
    # Number found, everything is fine. Continue interpreter loop.  Finished.

    # Number didn't like the token either.
    pushdadouble x10, x11
    #pushda x10
    #pushda x11
type_not_found_quit:
    call type
    writeln " not found."
    j quit

# -----------------------------------------------------------------------------
4:# Found token in dictionary. Decide what to do.

  # ( Token-Addr Token-Length )

  # Registers:
  #  x11: Flags
  #  x12: Code entry point
  #  x7: Constant folding pointer

  laf x13, state
  lc x13, 0(x13)
  bne x13, zero, 5f
    # Execute.
    laf x14, constantfoldingpointer
    li x7, 0   # Clear constant folding pointer
    sc x7, 0(x14)  # Do not collect literals for folding in execute mode. They simply stay on stack.

    li x15, Flag_immediate_compileonly & ~Flag_visible
    and x13, x11, x15
    bne x13, x15, execute_it
      call type
      writeln " is compile-only."
      j quit

execute_it:
    ddrop
    pushda x12    # Code entry point
    call execute  # Execute it
    j 1b          # Finished.

  # Registers:
  #  x10: From now on, this is number of constants that would be needed for folding this definition
  #  x11: Flags
  #  x13: Constant fill gauge of Datastack
  #  x12: Code entry point
  #  x7: Constant folding pointer

# -----------------------------------------------------------------------------
5:# In compile state.
    ddrop

    # Check this first, as ramallot is set together with foldability,
    # but the meaning of the lower 4 bits is different.

    andi x10, x11, Flag_ramallot & ~Flag_visible # Check flag field for foldability
    bne x10, zero, interpret_fold_optimization

    # Calculate number of folding constants available.

    sub x13, x7, x9 # Constant fill level indicator - Current stack pointer
    srli x13, x13, CELLSHIFT # Divide to get number of stack elements.
    # Number of folding constants now available in x13.

    # Check for foldability.

    andi x10, x11, Flag_foldable & ~Flag_visible # Check flag field for foldability
    beq x10, zero, constantloop

#       # Check for opcodability.
#       movs r0, #Flag_opcodable & ~Flag_visible
#       ands r0, r1
#       beq.n .interpret_enoughconstants # Flag is set
#       cmp r3, #0 # And at least one constant is available for folding.
#       beq.n .interpret_enoughconstants
#         b.n .interpret_opcodable

interpret_enoughconstants: # Not opcodable. Maybe foldable.
      # How many constants are necessary to fold this word ?
      andi x10, x11, 0x0F # Mask number of constants
      bltu x13, x10, constantloop

interpret_fold_optimization:
        # Do folding by running the definition.
        # Note that Constant-Folding-Pointer is already set to keep track of results calculated.
        pushda x12 # Code entry point
        call execute # Fold by executing
        j 1b # Finished.


constantloop:
    # No constant folding was possible, but perhaps we can generate a special opcode for this ?

    sltiu x10, x13, 1 # Only if there is at least one constant available
    bne zero, x10, 2f

      andi x10, x11, Flag_opcodable & ~Flag_visible # Check flag field for opcodeability
      beq x10, zero, 2f

        # Flags of Definition in x11
        # Entry-Point of Definition in x12
        # Number of folding constants available in x13

        pushda x12
        call findendofdefinition
        call execute
        j 1b

2:  # No optimizations possible. Compile the normal way.
    # Write all folding constants left into dictionary.

    call writeconstants

# -----------------------------------------------------------------------------

  # Check if writing a push x1 / pop x1 frame is necessary.

  laf x14, state
  lc x14, 0(x14)
  li x15, 1
  bne x14, x15, 2f

  andi x14, x11, Flag_noframe & ~Flag_visible
  bne x14, zero, 2f

    call push_x1_komma
    call compilemode
2:

# -----------------------------------------------------------------------------
  # Classic compilation.
  pushda x12 # Put code entry point on datastack.

  andi x12, x11, Flag_immediate & ~Flag_visible
  beq x12, zero, 6f
    # Always execute immediate definitions.
    call execute
    j 1b # Finished.

6:andi x12, x11, Flag_inline & ~Flag_visible
  beq x12, zero, 7f
    call inlinekomma # Inline the code
    j 1b # Finished.

7:call callkomma # Simply compile a call.
  j 1b # Finished.

# -----------------------------------------------------------------------------
writeconstants: # Special internal entry point with register dependencies.
# -----------------------------------------------------------------------------
  push x1
  beq x13, zero, 7f # Zero constants available ?
                    # Nothing to write.

constantsinnerloop:
    # Loop for writing all folding constants left.
    addi x13, x13, -1 # Because Pick addresses the topmost element with zero.
    pushda x13

    call pick
    call literalkomma

    bne x13, zero, constantsinnerloop

    # Drop constants written.

    addi x9, x7, -CELL # TOS was backed up when the constants were added.
    drop         # Retrieve the old TOS from its place on the stack.

7:laf x14, constantfoldingpointer
  li x7, 0   # Clear constant folding pointer.
  sc x7, 0(x14)
  pop x1
  ret


#------------------------------------------------------------------------------
  Definition Flag_visible|Flag_variable, "hook-quit" # ( -- addr )
  CoreVariable hook_quit
#------------------------------------------------------------------------------
  pushdaaddrf hook_quit
  ret
  .varinit quit_vanilla  # Simple loop for default

# -----------------------------------------------------------------------------
  Definition Flag_visible, "quit" # ( -- )
quit:
# -----------------------------------------------------------------------------
  # Clear stacks and tidy up.

  laf x15, gp_tp_sp
  lc sp, 8(x15)
  laf x9, __datastack # PSP

  laf x14, base
  li x15, 10      # Base decimal
  sc x15, 0(x14)

  laf x14, state
  li x15, 0       # Execute mode
  sc x15, 0(x14)

  laf x14, constantfoldingpointer
  # li x15, 0       # Clear constant folding pointer
  sc x15, 0(x14)

  laf x14, Bufferlevel
  # li x15, 0       # Set >IN to 0
  sc x15, 0(x14)

  laf x14, current_source
  # li x15, 0       # Empty TIB is source
  sc x15, 0(x14)
  laf x15, Inputbuffer
  sc x15, CELL(x14)

quit_intern:
  laf x15, hook_quit
  lc x15, 0(x15)

  jalr zero, x15, 0

# -----------------------------------------------------------------------------
  Definition Flag_visible, "(quit)" # ( -- )
quit_vanilla:  # Main loop of Forth system.
# -----------------------------------------------------------------------------
  call query
  call interpret
  writeln " ok."
  j quit_vanilla

