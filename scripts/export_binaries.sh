#|/bin/bash

# This script copies some key gateware bitstream files to the binaries/ directory to
# include in a release. The script assumes make has been run in the
# build/arty-a7-100 directory.
echo "Exporting bitstream binaries..."
echo "Note: This script should be sourced from a boxlambda workspace root directory."

mkdir -p ./binaries
rm -f ./binaries/*

cp ./build/arty-a7-100/gw/projects/boxlambda_base/project.bit ./binaries/boxlambda_base.bit
cp ./build/arty-a7-100/gw/projects/boxlambda_dfx/dfx_project.bit ./binaries/boxlambda_dfx.bit
cp ./build/arty-a7-100/gw/components/vs0_stub/vs0_stub_pblock_vs0_partial.bin.bin_for_icap ./binaries/vs0_stub.bin_for_icap
cp build/arty-a7-100/gw/components/vs0_j1b/vs0_j1b_pblock_vs0_partial.bin.bin_for_icap ./binaries/vs0_j1b.bin_for_icap

echo "Done."

