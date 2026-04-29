#!/usr/bin/env python3

import lz4.block
import argparse

def main(infile, outfile):
    with open(infile, "rb") as f:
        data = f.read()
    # store_size=False creates a pure raw block for LZ4_decompress_safe
    compressed = lz4.block.compress(data, store_size=False, mode='high_compression')
    with open(outfile, "wb") as f:
        f.write(compressed)

if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="lz4_compress.py <infile> <outfile> lz4 compresses <infile> to <outfile> for use width boxlambda lz4 decompressor."
    )

    parser.add_argument(
        "infile",
        help="Input file to be compressed"
    )

    parser.add_argument(
        "outfile",
        help="Compressed output file"
    )

    args = parser.parse_args()

    main(args.infile, args.outfile)

