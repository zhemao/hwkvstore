LDFLAGS=-g -lpthread
CFLAGS=-g -Wall

all: test calc_hash

test: test.o hash.o storage.o
	$(CC) $(LDFLAGS) $^ -o $@

calc_hash: calc_hash.o hash.o
	$(CC) $(LDFLAGS) $^ -o $@

clean:
	rm -f testhash *.o
