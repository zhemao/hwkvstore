#!/bin/bash
SHARED=$1
ROOT=$2


#if [ "$SHARED/$ROOT/.git/index" -ot "../.git/index" ]; then
  #echo "Deleting shared copy at $SHARED/$ROOT"
  #rm -rf "$SHARED/$ROOT"
  #echo "Copying from shared to local of o$LROOT"
  #mkdir -p $SHARED/$ROOT
  #cp -rf ".." "$SHARED/$ROOT"
#fi

mkdir -p $SHARED
mkdir -p $SHARED/$ROOT
mkdir -p $SHARED/$ROOT/results
rsync -au --exclude="*.git" --exclude="*/riscv-gcc/*" --exclude="*/riscv-llvm/*" --exclude="*/target/*" ".." "$SHARED/$ROOT"
