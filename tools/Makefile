CC=riscv64-unknown-elf-gcc
CFLAGS=-Wall -O2

all: switch-mode.rv set-key.rv test.hex

test.hex: test.rv
	elf2hex 16 16384 test.rv > test.hex

test.rv: test.S
	$(CC) -nostdlib -T ../../riscv-tools/riscv-tests/env/p/link.ld $< -o $@

switch-mode.rv: switch-mode.c
	$(CC) $(CFLAGS) $^ $(LDFLAGS) -o $@

set-key.rv: set-key.c accel.o
	$(CC) $(CFLAGS) $^ $(LDFLAGS) -o $@

%.o: %.c
	$(CC) $(CFLAGS) -c $<

clean:
	rm -f *.o *.rv
