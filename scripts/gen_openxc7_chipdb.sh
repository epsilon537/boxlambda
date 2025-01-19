#! /bin/bash

mkdir -p chipdb
pushd .
cd chipdb

echo "bbaexporting xca35t"
${PYPY3} ${NEXTPNR_XILINX_PYTHON_DIR}/bbaexport.py --device xc7a35tcsg324-1 --bba xc7a35tcsg324.bba
echo "bbaasm xca35t"
bbasm -l xc7a35tcsg324.bba xc7a35tcsg324.bin
rm -f xc7a35tcsg324.bba

echo "bbaexporting xca100t"
${PYPY3} ${NEXTPNR_XILINX_PYTHON_DIR}/bbaexport.py --device xc7a100tcsg324-1 --bba xc7a100tcsg324.bba
echo "bbaasm xca100t"
bbasm -l xc7a100tcsg324.bba xc7a100tcsg324.bin
rm -f xc7a100tcsg324.bba

popd

