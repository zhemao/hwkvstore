CC=riscv64-unknown-elf-gcc
CFLAGS=-Wall -O2

all: switch-mode.rv set-key.rv

switch-mode.rv: switch-mode.c
	$(CC) $(CFLAGS) $^ $(LDFLAGS) -o $@

set-key.rv: set-key.c accel.o
	$(CC) $(CFLAGS) $^ $(LDFLAGS) -o $@

%.o: %.c
	$(CC) $(CFLAGS) -c $<

clean:
	rm -f *.o *.rv
