
# verilator
export PATH=$PATH:/tools/projects/stevo/craft/chisel3/verilator/install/bin
export VERILATOR_ROOT=/tools/projects/stevo/craft/chisel3/verilator/install/share/verilator


# vcs
export PATH=/tools/synopsys/vcs/J-2014.12-SP1/bin:$PATH
export VCS_HOME=/tools/synopsys/vcs/J-2014.12-SP1/
export VCS_64=1

export RISCV=/tools/projects/hurricane2/tools/riscv-tools-1
export PATH=$RISCV/bin:$PATH

if test -f /opt/rh/devtoolset-2/enable
then
  source /opt/rh/devtoolset-2/enable
fi
