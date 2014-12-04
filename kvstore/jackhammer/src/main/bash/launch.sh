#!/bin/bash
#PBS -N hammer
#PBS -q batch
#PBS -l nodes=1:ppn=8
#PBS -l vmem=20gb,mem=20gb,pmem=20gb
#PBS -l walltime=03:00:00
###PBS -m e
###PBS -M adamiz@eecs.berkeley.edu
#PBS -m n
#PBS -e .
#PBS -o .

###export DESIGN=**DESIGN** #design point tag (not including .prm suffix)
###export SHARED=**SHARED** #shared directory in /nscratch/ to store runs and index
###export ROOT=**ROOT**   #name of repo, e.g. reference-chip
###export LOCAL=**LOCAL**  #local directory in /scratch/ to store all runs
###export LROOT=**LROOT**  #local root, some sort of index of runs 



#if [ -d "$SHARED/$ROOT" ]; then
  #git clone $GIT "$SHARED/$ROOT"
  #cd "$SHARED/$ROOT" && git submodule update --init --recursive
  #cd "$SHARED/$ROOT" && git checkout $BRANCH
  #cd "$SHARED/$ROOT/chisel" && git checkout $BRANCH
#fi

echo "Starting Job"
echo Running on host `hostname`
echo "SHARED: $SHARED"
echo "ROOT: $ROOT"
echo "LOCAL: $LOCAL"
echo "CONFIG: $CONFIG"

#export SBT="java -Xmx2048M -Xss8M -XX:MaxPermSize=128M -jar $SHARED/$ROOT/sbt-launch.jar"
export ME=`whoami`
#export PATH="$PATH:$RISCV/bin"
echo WHOAMI: $ME

#echo "LROOT: $LROOT"
#mkdir -p $SHARED/out
#ps aux > $SHARED/out/$LROOT.txt
#export LROOT=`perl $SHARED/$ROOT/jackhammer/child/set_socket.pl $SHARED/out/$LROOT.txt $ME`
#echo "LROOT: $LROOT"

echo "mkdir -p $LOCAL"
mkdir -p $LOCAL
echo "mkdir -p $LOCAL/$LROOT"
mkdir -p $LOCAL/$LROOT
echo "rsync -au --exclude=\"*/results/*\" --exclude \"*/outputs/*\" \"$SHARED/$ROOT\" \"$LOCAL/$LROOT\""
rsync -au --exclude="*/results/*" --exclude "*/outputs/*" "$SHARED/$ROOT" "$LOCAL/$LROOT"
echo "cd \"$LOCAL/$LROOT/$ROOT/jackhammer/\" && make child"
cd "$LOCAL/$LROOT/$ROOT/jackhammer/" && make child

#mkdir -p $LOCAL/o$LROOT
#cp -rfu "$SHARED/$ROOT" "$LOCAL/o$LROOT"
#mv "$LOCAL/o$LROOT" "$LOCAL/b$LROOT"
#cd "$LOCAL/b$LROOT/$ROOT/jackhammer/" && make child
#mv "$LOCAL/b$LROOT" "$LOCAL/o$LROOT"
