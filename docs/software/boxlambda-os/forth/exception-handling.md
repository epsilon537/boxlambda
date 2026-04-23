# Forth Exception Handling

The following diagram illustrates Word `foo` successfully *trying* `bar`, i.e. bar does not raise an exception.

[![Foo Successfully tries bar..](../../../assets/foo-tries-bar-no-exception.png)](../../../assets/foo-tries-bar-no-exception.png)

*Foo Successfully tries bar.*

In the next diagram, `foo` again *tries* `bar`, but this time, `bar` raises an exception called `x-y-z`:

[![Foo tries bar with exception.](../../../assets/foo-tries-bar-throws-exception.png)](../../../assets/foo-tries-bar-throws-exception.png)

*Foo tries bar, with exception.*

Raising an exception outside of a try block will end up *restoring* the state to whatever the `ExcStackFramePtr` variable happens to be pointing to.
To avoid unpleasant surprises, the top-level REPL, i.e. the quit loop, is put within a try-block.

## Caveat

A caveat to keep in mind: A raised exception restores the Data Stack to the point before the try call.
This means that within a try-block, code that might throw an exception should not manipulate data stack items *outside* of the try-block.

For example:

```
: x-y-z ." x-y-z exception raised." cr;

: double-it ( n -- n')
  2*
  ['] x-y-z ?raise
;

: foo ( -- n )
    3
    [: dup double-it ;] try ( n exception-xt )
    drop ( n )
;

: bar ( -- n )
    3 dup
    [: double-it ;] try ( n exception-xt )
    drop ( n )
;

foo . cr
bar . cr
```

`Foo . cr` will print the value 3. However, `Bar . cr` will print the value 6 because in bar's case, `double-it` reaches outside of the data stack frame restored by `?raise`.

