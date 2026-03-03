# BoxLambda port of ZeptoForth's exception handling mechanism
# by Ruben Lysens/Epsilon527.
#
# Original header:
#
# copyright (c) 2019-2023 travis bemann
#
# permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "software"), to deal
# in the software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the software, and to permit persons to whom the software is
# furnished to do so, subject to the following conditions:
#
# the above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the software.
#
# the software is provided "as is", without warranty of any kind, express or
# implied, including but not limited to the warranties of merchantability,
# fitness for a particular purpose and noninfringement. in no event shall the
# authors or copyright holders be liable for any claim, damages or other
# liability, whether in an action of contract, tort or otherwise, arising from,
# out of or in connection with the software or the use or other dealings in the
# software.

# -----------------------------------------------------------------------------
  Definition Flag_visible, "?raise" # ( xt|0 -- | 0 )
_raise: # Raise an exception with the exception type in the TOS register.
# -----------------------------------------------------------------------------
  beq x8, zero, 1f
  laf x14, ExceptionFramePointer
  lc x15, 0(x14)
  mv sp, x15      # Switch SP to ExceptionFrame.
  pop x15         # Get previous ExceptionFramePointer from Exception Frame
  sc x15, 0(x14)  # Make it the current ExceptionFramePointer, i.e. restore the exception chain.
  popdouble x9 ra # Switch PSP and RA to PSP and RA store in ExceptionFrame.
                  # This means, we'll be returning to try's caller.
  ret
1: # No exception.
  drop
  ret

# -----------------------------------------------------------------------------
  Definition Flag_visible, "try" # ( xt1 -- xt2|0 )
_try: # Try to see if an exception occurs
# -----------------------------------------------------------------------------
  push x1 # Create an ExceptionStackFrame, consisting of caller's RA,...
  push x9 # ... the PSP,...
  laf x14, ExceptionFramePointer
  lc x15, 0(x14)
  push x15 # ...and the current Exception Frame Pointer.
  sc sp, 0(x14) # Make the next Exception Frame current
  popda x15 # Call the xt on the datastack
  jalr ra, x15
  laf x14, ExceptionFramePointer # If we returned here, no exception occured.
  pop x15 # This and the next two pops remove the created ExceptionStackFrame.
  sc x15, 0(x14) # Restore previous ExceptionFramePointer.
  pop x0 # Pop and discard the exception frame's saved PSP.
  pushda x0 # TOS=0
  pop x1
  ret

