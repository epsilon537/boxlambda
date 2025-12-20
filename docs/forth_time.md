# Time Handling

## Time Handling Words

```
        mtime64         ( -- ud ) Get 64-bit mtime
        mtimecmp64      ( -- ud ) Get 64-bit mtime comparator
        mtimecmp64!     ( ud -- ) Set 64-bit  mtime comparator
        set-raw-time-cmp! ( offset - ) Set mtime comparator using an offset relative to current time.
        cycles64        ( -- ud ) Get 64-bit uptime in cycles
```

## Example

See [Interrupt Handling Example](forth_irqs.md#interrupt-handling-example).

