jackhammer!

Config options:
set cpp		\\ Only run C++ emulator
set vsr		\\ Only build verilog design and run vcs-sim-rtl
set tst		\\ Run both C++ and Verilog backends
set syn		\\ Run cpp, verilog, vcs-sim-rtl, and dc-syn
set par		\\ Run cpp, verilog, vcs-sim-rtl, dc-syn, and icc-par
set ptp		\\ Run cpp, verilog, vcs-sim-rtl, dc-syn, icc-par, vcs-sim-gl-par, and pt-power

set search x	\\ Search exhaustively. Currently, there are no other options.

set dump	\\ Before launching jobs, dump out parameter space from current chisel design. If not set, will use existing generated/space.prm

set run <name>	\\ Will create directory of specified name

set new		\\ If run directory exists, will overwrite existing files. Otherwise, will continue where it left off by reading existing run.idx
===

Design space exploration toolkit for Chisel
Quick and dirty instructions:

For reference chip:
     ***Note: All commands are executed from base directory***
 1.  Checkout git repo and chisel repo to branch dse
         git checkout dse
         //cd chisel && git checkout dse
 2.  Parameterize design
   e.g.  val isize = RangeParam("i",7,7,9)
         val ic = ICacheConfig(math.pow(2, isize.getValue).toInt, 2, ntlb = 8, nbtb = 38)
 3.  Dump exhaustive parameter space
         cd jack && make dump
 4.  Edit variables in hammer/base.in
   e.g.  SHARED /nscratch/<username>/hammer
         ROOT reference-chip
         LOCAL /scratch/<username>/hammer
         RISCV /nscratch/<username>/hammer/install
 5.  Add desired designs to space.in
         ls jackhammer/designs > jackhammer/inputs/designs.in
 6.  Write all scripts for each qor
 7.  Add all script pointers for each qor
   e.g.  echo "cpp -exe ../scripts/cpp/ex.sh -exeReg ERROR -res ../scripts/cpp/re.sh -resReg ERROR
               ver -exe ../scripts/ver/ex.sh -exeReg ERROR -res ../scripts/ver/re.sh -resReg ERROR
	       vcs -exe ../scripts/vcs/ex.sh -exeReg ERROR -res ../scripts/vcs/re.sh -resReg ERROR" > hammer/scripts.in
 8.  Add desired subset of qors to qors.in
         echo "QORS cpp ver vcs" > hammer/qors.in
 9.  Construct configure.in
         cat base.in qors.in scripts.in space.in > configure.in
 10. Initialize base repo in nscratch
   e.g.  cd /nscratch/<username>/hammer/ && git clone <git_url_to_reference_chip> reference-chip && git checkout dse
 11. (If needed by qors) Install RISCV tools in nscratch
   e.g.  export RISCV=/nscratch/<username>/hammer/install
         ./riscv-tools/build.sh 
 12. (If needed by qors) Update hammer/launch.sh qsub resource requirements
 13. Start run
         cd hammer && ./init.sh
 14. Update index periodically
         cd hammer && ./update.sh
    
TODO:
  Hardcode qsub.sh into hammer.scala, delete from configure.in
  create base.in
  add run index name to configure.in

Jack:
  Add min/max as input parameters
  Add power2
  Add enums


