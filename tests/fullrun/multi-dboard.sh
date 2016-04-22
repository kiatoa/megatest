#!/bin/bash

if [[ ! -e "$HOME/.megatest" ]];then
   mkdir -p "$HOME/.megatest"
fi
# if [[ ! -e "$HOME/.megatest/areas.dat" ]];then
#    echo "Creating some placeholder files in ~/.megatest"
#    cat > "$HOME/.megatest/areas.dat" << EOF
# [default]
# mfstest /mfs/matt/data/megatest/tests/fullrun
# mfsbig /mfs/matt/data/megatest/tests/fdktestqa/testqa
# [local]
# localtest /home/matt/data/megatest/tests/fullrun
# EOF
# fi
if [[ ! -e "$HOME/.megatest/default.dat" ]];then
   cat > "$HOME/.megatest/default.dat" << EOF
[fullrun]
path /p/fdk/gwa/ritikaag/megatest/tests/fullrun
order 1
# [bigrun]
# path /mfs/matt/data/megatest/tests/fdktestqa/testqa
# order 2
# [local_fullrun]
# path /home/matt/data/megatest/tests/fullrun
# order 3
EOF
fi

/nfs/pdx/disks/icf_external/pkgs/chicken/4.10.0/bin/csi -I ../.. multi-dboard-load-all.scm
