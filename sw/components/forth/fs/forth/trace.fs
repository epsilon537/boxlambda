0 variable trace-saved-hook-interpret

: trace_on
  ." Enabling tracing..." cr
  hook-interpret @ trace-saved-hook-interpret !
  [: ." -> " source type ."  <- " cr trace-saved-hook-interpret @ execute ;] hook-interpret !
;

: trace_off
  trace-saved-hook-interpret @ ?dup if
    hook-interpret !
    0 trace-saved-hook-interpret !
    ." Disabled tracing." cr
  then
;
