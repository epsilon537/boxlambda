---
hide:
  - toc
---

## Ymodem

- **Xtreamerdev Repo**:
    [https://github.com/pbatard/xtreamerdev](https://github.com/pbatard/xtreamerdev)

- **Ymodem Software Component in the BoxLambda Directory Tree**:
  [boxlambda/sw/components/ymodem](https://github.com/epsilon537/boxlambda/tree/master/sw/components/ymodem)

- **Ymodem CLI Component in the BoxLambda Directory Tree**:
  [boxlambda/sw/components/ymodem_cli](https://github.com/epsilon537/boxlambda/tree/master/sw/components/ymodem_cli)

This software component is a BoxLambda port of Xtreamerdev's Ymodem protocol implementation. The Ymodem API enables file transfers over the serial port. The `ymodem_receive()` function initiates a Ymodem transfer in BoxLambda's receive direction, storing the received file data in a memory buffer. The `modem_transmit()` function transfers the contents of a given memory buffer to the host PC using the Ymodem protocol.

`ymodem.h`:

```
void ymodem_init(struct uart *uart);
unsigned long ymodem_receive(unsigned char *buf, unsigned long length);
unsigned long ymodem_send(unsigned char *buf, unsigned long size, char* filename);
```

The `ymodem` software component is used by the `ymodem_cli` CLI component to transfer files between the host PC's file system and BoxLambda's file system:

```
* ymodem_rx
    ymodem_rx <filename>: Ymodem rx and save to give file.
* ymodem_tx_buf
    ymodem_tx_buf <filename> <hex address> <size_in_bytes>: Ymodem transmit given memory buffer with given filename
```


