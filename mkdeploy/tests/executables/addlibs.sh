#!/bin/bash -e

CSIPATH=$(echo $(type csi)|awk '{print $3}')
CKPATH=$(dirname $(dirname $CSIPATH))
rsync -av $EXECUTABLE/ ../deploytarg/
for i in iup im cd av call sqlite; do
  cp $(CKPATH)/lib/lib$i* ../deploytarg/
done

cp $(CKPATH)/include/*.h ../deploytarg/
