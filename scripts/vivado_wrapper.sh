#! /bin/bash

#By default Vivado generates quite a lot of logs and traces.
#The flags below turn that off.
#When I'm debugging build issues, I just unset VIVADO_FLAGS.
VIVADO_FLAGS="-nolog -nojournal -notrace"

vivado -mode batch $VIVADO_FLAGS $*

