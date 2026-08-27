\ Hook in here any clean-up etc. that needs to happen whenever quit is invoked.
\ The custom quit-hook in prompt.fs will invoke this chain prior to entering the
\ quit loop.
\ This variable is defined early in the include list to allow early boxkern-includes
\ to install their on-quit hooks (see. compileto.fs for example).

' nop variable hook-on-quit
