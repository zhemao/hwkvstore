CC=riscv64-unknown-elf-gcc
CFLAGS=-Wall -O2

set-key.rv: set-key.c accel.o accel-inst.h
	$(CC) $(CFLAGS) $^ $(LDFLAGS) -o $@

%.o: %.c
	$(CC) $(CFLAGS) -c $<

clean:
	rm -f *.o *.rv
