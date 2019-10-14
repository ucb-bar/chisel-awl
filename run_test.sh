#!/bin/sh
# TODO 2019-4-16 This is using the old rocket-chip tests until the chisel-testers repo supports verilator

set -e

if [ $# -ne 2 ]; then
  echo "Usage: ./run_test.sh PROJECT CONFIG"
  exit 1
fi
PROJECT=$1
MODEL=TestHarness
CONFIG=$2
long_name=${PROJECT}.${MODEL}.${CONFIG}
GENSRC=generated-src/${long_name}
mkdir -p ${GENSRC}
sbt "dumpCsrc"
sbt "runMain awl.Generator $GENSRC $PROJECT $MODEL $PROJECT $CONFIG"
sbt "runMain firrtl.Driver -o ${GENSRC}/${long_name}.v -i ${GENSRC}/${long_name}.fir -faf ${GENSRC}/${long_name}.anno.json -td ${GENSRC}"
touch ${GENSRC}/firrtl_black_box_resource_files.f
verilator \
  --cc --exe \
  --top-module ${MODEL} \
  --assert \
  --output-split 20000 \
  -o sim -Mdir ${GENSRC} \
  +define+STOP_COND='$c("verbose","&&","done_reset")' \
  +define+PRINTF_COND='$c("done_reset")' \
  -Wno-STMTDLY -Wno-WIDTH --x-assign unique \
  -f ${GENSRC}/firrtl_black_box_resource_files.f -f sim_files.f \
  -O3 -CFLAGS "-O1 -std=c++11 -D__STDC_FORMAT_MACROS -DTEST_HARNESS=V${MODEL} -DVERILATOR" \
  -CFLAGS "-include ${long_name}.plusArgs -include V${MODEL}.h" \
  ${GENSRC}/${long_name}.v

make VM_PARALLEL_BUILDS=1 -C ${GENSRC} -f V${MODEL}.mk
cd ${GENSRC} && ./sim
